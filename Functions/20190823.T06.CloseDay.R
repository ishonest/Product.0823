IB.Missed.Orders <- function(today = Sys.Date())
{
  d1 <- foreach(ticker = unique(IB.01.targets$ticker),
                .combine = bind_rows, .errorhandling = 'remove') %do%
    {
      d1 <- get.clean.data(ticker, src = "yahoo", first.date = today, last.date  = today + 1) %>%
        rename(ds = ref.date, open = price.open, high = price.high,
               low = price.low, close = price.close) %>%
        select(ticker, ds, open, high, low, close)

      rm(ticker)
      return(d1)
    }

  x1 <- IB.01.targets %>%
        left_join(d1, by = c("ticker", "ds")) %>%
        mutate(action = case_when(Type == "LONG" & invest > 0 & 
                                    low <= buy.price & low > stop.price ~ "BUY"
                                  , Type == "LONG" & action == "HOLD" & 
                                    sell.price <= high ~ "SELL"
                                  , Type == "LONG" & action == "HOLD" & 
                                    low <= stop.price ~ "STOP SELL"
                                  , Type == "LONG" & action == "HOLD" & 
                                    (!is.na(last.sell) & last.sell >= 0) ~ "EOD SELL"
                                  
                                  , Type == "SHRT" & invest > 0 & 
                                    high >= buy.price & high < stop.price ~ "BUY"
                                  , Type == "SHRT" & action == "HOLD" & 
                                    sell.price >= low ~ "SELL"
                                  , Type == "SHRT" & action == "HOLD" & 
                                    high >= stop.price ~ "STOP SELL"
                                  , Type == "SHRT" & action == "HOLD" & 
                                    (!is.na(last.sell) & last.sell >= 0) ~ "EOD SELL"
        )) %>%
        filter(!is.na(action))
  
  position <- IB.04.activity %>% 
              group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
              summarise(model.position = 1.0*sum(ifelse(IB.action == "BUY", volume, -volume))) %>%
              filter(model.position > 0) %>%
              left_join(IB.00.positions %>% rename(ticker = symbol) %>%
                          group_by(ticker) %>% summarise(ticker.position = sum(position))
                        , by = "ticker") %>%
              filter(ticker.position >= model.position) %>% ungroup()
  
  x2 <- left_join(x1, position, 
                  by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
        filter(grepl("BUY", action) | (grepl("SELL", action) & model.position > 0)) %>%
        mutate(t.price = case_when(action %in% c("BUY", "SELL") ~ round(close, 2))
               , m.price = case_when(action == "SELL" ~ sell.price,
                                     action == "STOP SELL" ~ stop.price,
                                     action == "BUY" ~ buy.price )
               , volume = case_when(grepl("SELL", action) ~ model.position,
                                    grepl("BUY", action) ~ floor(invest/t.price))
        ) %>%
        select(ticker, ds, Type, action, volume, m.price, 
               algoId, ID, DP.Method, MA.Type, Period, tickerID, Exchange)
  

  assign("IB.06.missed.orders", x2, envir = .GlobalEnv)
  rm(today, d1, x1, x2)

}

IB.FinishDay <- function(Force.Close = FALSE)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(!(NY.Time > 16.00 | Force.Close))
  {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
  rm(NY.Time)

  IB.Cancel.Orders()
  IB.Missed.Orders()

  h.missed <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/06.Historical.Misses.rds")) %>%
    bind_rows(IB.06.missed.orders) %>% distinct()

  h.activity <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds")) %>%
    bind_rows(IB.04.activity) %>% distinct() %>% arrange(desc(order.ts))

  h.orders <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds")) %>%
    bind_rows(IB.03.orders) %>% distinct() %>% arrange(desc(order.ts))

  saveRDS(h.missed, paste0(IB.Parms[["data.folder"]], "Trading/06.Historical.Misses.rds"))
  saveRDS(h.activity, paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds"))
  saveRDS(h.orders, paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds"))

  rm(h.missed, h.activity, h.orders)
  rm(list = setdiff(ls(envir = .GlobalEnv), c("tws")), envir = .GlobalEnv)
}

IB.FinishDay()

twsDisconnect(tws)
rm(list = ls())
