Update.Activity <- function()
{
  source("./Functions/F.Additional.IB.R")
  
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
      # There is a 2 second clock diff of IB / System
      filter(order.ts > IB.Parms[["Last.Order.Time"]] - 2) %>%
      mutate(conId = as.character(conId)
             , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
             , IB.action = case_when(action == "BOT" ~ "BUY",
                                  action == "SLD" ~ "SELL")
      ) %>%
      group_by(orderId, IB.action, ticker) %>%
      summarise(order.ts = max(order.ts, na.rm = TRUE),
                shares = sum(shares),
                price = weighted.mean(avgPrice, W = shares)) %>%
      ungroup()
  )

  if(nrow(x) == 0) 
  {
    rm(x)
    return(cat("\nThere were NO Recent Activity ... "))
  }
  
  
  ############################# In.Hand Portfolio / From Activity #############################
  y <- IB.04.activity %>% group_by(algoId, ticker, Type, DP.Method, MA.Type, Period) %>%
        filter(order.ts == max(order.ts)) %>% 
        summarise(in.hand = sum(volume, na.rm = TRUE ))
  
  ############################# Updating IB Activity #############################
  if(IB.Parms[["Emergency"]])
  {
    z <- inner_join(y, x, by = c("ticker")) %>%
          group_by(orderId, ticker, Type) %>%
          mutate(Situation = "Emergency"
                 , in.hand = abs(in.hand)
                 , shares = abs(shares)
                 , left = shares - (cumsum(in.hand) - in.hand)
                 , shares = pmin(left, in.hand)
                 , volume = case_when(IB.action == "BUY" ~ shares,
                                      IB.action == "SELL" ~ -shares)
          ) %>%
          select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, 
                 IB.action, orderId, order.ts, volume, price)
    
  } else
  {
    z <- inner_join(x, IB.02.actions %>% 
                     select(IB.action, ticker, Type, action, volume, algoId, DP.Method, MA.Type, Period)
                   , by = c("IB.action", "ticker")) %>%
          group_by(orderId, IB.action, ticker) %>%
          mutate(Situation = "Normal",
                 left = shares - (cumsum(volume) - volume),
                 shares = pmin(left, volume),
                 volume = case_when(IB.action == "BUY" ~ shares,
                                    IB.action == "SELL" ~ -shares)
          ) %>%
          select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation,
                 IB.action, orderId, order.ts, volume, price)
  }
  
  ############################# For Manual Override #############################
  if(nrow(z) == 0 & nrow(x) > 0)
  {
    z <- left_join(x, y, by = c("ticker")) %>% 
          mutate(Situation = "Manual",
                 volume = case_when(IB.action == "BUY" ~ shares,
                                    IB.action == "SELL" ~ -shares)
                 ) %>% 
          select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, 
                 IB.action, orderId, order.ts, volume, price)  
  }
  
  # Sanity Check
  z <- z %>% filter((IB.action == "BUY" & volume > 0) | (IB.action == "SELL" & volume < 0))
  
  z <- bind_rows(z, IB.04.activity) %>%
        ungroup() %>% arrange(desc(order.ts)) %>% distinct()
  
  assign("IB.04.activity", z, envir = .GlobalEnv)
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  rm(x, y, z)
}

Update.Targets <- function()
{
  if(IB.Parms[["Emergency"]]) {return(cat("\n\nSystem halted due to Emergency"))}
  
  x1 <- IB.04.activity %>% distinct() %>% 
        group_by(ticker, algoId, Type, DP.Method, MA.Type, Period) %>%
        filter(order.ts == max(order.ts)) %>%
        summarise(invested = abs(volume)*price)
  
  # For Partial Buy Fulfillment
  # Partial Sell Fulfillment will be taken care by IB.actions
  x2 <- left_join(IB.01.targets, x1
                  , by = c("ticker", "algoId", "Type", "DP.Method", "MA.Type", "Period")) %>%
        mutate(balance = invest - invested) %>%
        filter(is.na(balance) | balance >= IB.Parms[["invest.min"]] ) %>%
        mutate(invest = ifelse(is.na(balance), invest, balance)) %>%
        select(-c(invested, balance))

  assign("IB.01.targets", x2, envir = .GlobalEnv)
  rm(x1, x2)
}
