AF.Manual.Activity <- function()
{
  First.Order <- as.POSIXct(paste(Sys.Date(), "09:30:00"), tz = "America/New_York")
  
  source("./Functions/20190823.T05.X.Messaging.R")

  good.run <- FALSE
  while(!good.run)
  {
    x <- tryCatch({req.Exec.df(tws)}
                  , warning = function(w) {0}
                  , error = function(e) {0} )
    if(is.data.frame(x) | is.null(x)) {good.run <- TRUE}
  }
  rm(good.run)

  rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)

  if(is.null(x)) {return(cat("\nThere were NO Recent Activity ... "))}

  suppressWarnings(
    x <- x %>%
      select(conId, orderId, symbol, side, time, shares, price, avgPrice) %>%
      rename(ticker = symbol, action = side) %>%
      mutate(order.ts = as.POSIXct(time, format="%Y%m%d %H:%M:%S",tz = Sys.timezone())) %>%
      # There is a 4-5 second clock diff of IB / System: Used 10 second for safe side
      filter(order.ts > First.Order - 10) %>%
      mutate(conId = as.character(conId)
             , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
             , IB.action = case_when(action == "BOT" ~ "BUY",
                                     action == "SLD" ~ "SELL")
      ) %>%
      group_by(IB.action, ticker) %>%
      summarise(order.ts = max(order.ts, na.rm = TRUE),
                volume.executed = sum(shares),
                price = weighted.mean(avgPrice, W = shares)) %>%
      ungroup()
  )

  if(nrow(x) == 0)
  {
    rm(x)
    return(cat("\nThere were NO Activity Today ... "))
  }

  x1 <- IB.04.activity %>% filter(order.ts >= First.Order - 10) %>%
        group_by(ticker, IB.action) %>% 
        summarise(volume.accounted = abs(sum(units))) %>%
        full_join(x, by = c("ticker", "IB.action")) %>%
        mutate(volume.accounted = ifelse(is.na(volume.accounted), 0, volume.accounted),
               volume.executed = ifelse(is.na(volume.executed), 0, volume.executed),
               volume.left = volume.executed - volume.accounted
               ) %>%
        filter(volume.left > 0) %>%
        select(-c(volume.accounted, volume.executed)) %>%
        rename(volume.executed = volume.left)

  x2 <- IB.01.targets %>%
        mutate(Situation = "Manual", 
               IB.action = ifelse(units > 0, "BUY", "SELL"),
               volume = abs(units)
               ) %>%
        select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, IB.action, volume) %>%
        inner_join(x1, by = c("ticker", "IB.action")) %>%
        group_by(ticker, IB.action) %>% 
        arrange(ticker, IB.action, DP.Method, MA.Type, Period) %>%
        mutate(vol.cum = cumsum(volume),
               vol.left = ifelse(volume.executed >= vol.cum, 0, vol.cum - volume.executed),
               vol.left = pmin(volume, vol.left),
               vol.real = volume - vol.left,
               units = ifelse(IB.action == "BUY", vol.real, -vol.real)
        ) %>%
        filter(vol.real > 0) %>%
        select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, IB.action, 
               order.ts, units, price) %>% ungroup()
  
  x3 <- bind_rows(x2, IB.04.activity) %>% distinct()

  assign("IB.04.activity", x3, envir = .GlobalEnv)
  rm(x, x1, x2, x3, First.Order)
  gc()

}

AF.Performance.Summary <- function()
{
  
  d1 <- foreach(ticker = unique(IB.04.activity$ticker), .packages = "BatchGetSymbols"
                , .combine = bind_rows, .errorhandling = 'remove') %do%
    {
      d1 <- get.clean.data(ticker, src = "yahoo", first.date = max(IB.01.targets$ds)
                           , last.date  = max(IB.01.targets$ds) + 1
                           ) %>%
              rename(close = price.close) %>% select(ticker, close)
      
      rm(ticker)
      return(d1)
    }

  x <- IB.04.activity %>%
        mutate(order.ds = as.Date(order.ts)) %>%
        group_by(algoId, ticker, Type, DP.Method, MA.Type, Period) %>%
        summarise(returns = sum(price*units), units = sum(units)) %>%
        left_join(d1, by = "ticker") %>%
        mutate(returns = units*close - returns) 
  
  y <- x %>% group_by(Type) %>%
        summarise(Trades = n(),
                  SR = sum(returns > 0)/Trades,
                  Returns = sum(returns),
                  ROI = Returns/IB.Parms[["invest.max"]]
                  ) %>%
        bind_rows(x %>% ungroup() %>%
                    summarise(Type = "Overall",
                              Trades = n(),
                              SR = sum(returns > 0)/Trades,
                              Returns = sum(returns),
                              ROI = Returns/IB.Parms[["invest.max"]] )
                  ) %>%
        mutate(Returns = paste(formatC(Returns, format="f", big.mark=",", digits=0), "USD"),
               ROI = paste0(formatC(100*ROI, format="f", big.mark=",", digits=2), "%"),
               SR = paste0(formatC(100*SR, format="f", big.mark=",", digits=2), "%")) %>%
        rename("Gross Returns" = Returns, "Gross ROI" = ROI, "Success Rate" = SR)
  
  cat("\n")
  print(knitr::kable(y, align = 'r'))
  cat("\n")
  
  rm(x, y, d1)
  
}

IB.Shutter.Down <- function(Force.Close = FALSE)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(!(NY.Time > 16.00 | Force.Close))
  {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
  rm(NY.Time)

  IB.Account.Status()
  IB.Cancel.Orders()
  Update.Activity()
  AF.Manual.Activity()
  AF.Performance.Summary()

  h.activity <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds")) %>%
                bind_rows(IB.04.activity) %>% distinct() %>% arrange(desc(order.ts))

  h.latest <- h.activity %>%
              group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
              summarise(units = sum(units, na.rm = TRUE)) %>%
              filter(units != 0)

  h.orders <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds")) %>%
              bind_rows(IB.03.orders) %>% distinct() %>% arrange(desc(order.ts))
  
  View(h.activity)
  View(h.latest)

  saveRDS(h.latest, paste0(IB.Parms[["data.folder"]], "Trading/00.Latest.rds"))
  saveRDS(h.activity, paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds"))
  saveRDS(h.orders, paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds"))

  do.call(file.remove, list(list.files(paste0(IB.Parms$data.folder, "Simulation/"), full.names = TRUE)))
  rm(h.activity, h.orders, h.latest, Force.Close)
}

IB.Shutter.Down()

twsDisconnect(tws)
rm(list = ls())
gc()