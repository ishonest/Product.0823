Update.Activity <- function()
{
  # if(!exists("IB.02.actions", envir = .GlobalEnv) || !IB.Parms[["Emergency"]] & nrow(IB.02.actions) == 0)
  # {return(cat("\nThere are NO Activity Updates ... "))}
  
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
  
  if(is.null(x)) 
  {
    rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
    return(cat("\nThere were NO Recent Activity ... "))
  }
  
  suppressWarnings(
    x <- x %>%
      select(conId, orderId, symbol, side, time, shares, price, avgPrice) %>%
      rename(ticker = symbol, action = side) %>%
      mutate(order.ts = as.POSIXct(time, format="%Y%m%d %H:%M:%S",tz = Sys.timezone())) %>%
      # There is a 2 second clock diff of IB / System
      filter(order.ts > IB.Parms[["Last.Order.Time"]] - 2) %>%
      mutate(conId = as.character(conId)
             , NY.time = as.numeric(strftime(format(order.ts, tz = "US/Eastern"), format = "%H.%M"))
             , shares = case_when(action == "BOT" ~ shares,
                                  action == "SLD" ~ -shares)
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
    rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
    return(cat("\nThere were NO Recent Activity ... "))
  }
  
  ############################# In.Hand Portfolio / From Activity #############################
  y <- IB.04.activity %>% group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
        filter(order.ts == max(order.ts)) %>% 
        summarise(in.hand = sum(volume, na.rm = TRUE ))
  
  ############################# Updating IB Activity #############################
  if(IB.Parms[["Emergency"]])
  {
    z <- inner_join(y, x, by = c("ticker")) %>%
          group_by(ticker, IB.action) %>%
          mutate(cumvol = cumsum(in.hand)
                 , sold = case_when(cumvol <= shares ~ in.hand,
                                    cumvol - shares < in.hand ~ cumvol - shares,
                                    TRUE ~ 0)
                 , in.hand = in.hand - sold
                 , volume = sold
                 , Situation = "Emergency"
          ) %>%
          select(algoId, ticker, DP.Method, MA.Type, Period, Situation, IB.action, 
                 orderId, order.ts, volume, price)
    
  } else
  {
    z <- left_join(IB.02.actions, x, by = c("ticker", "IB.action")) %>%
          filter(!is.na(orderId)) %>%
          mutate(Situation = "Normal") %>%
          select(algoId, ticker, DP.Method, MA.Type, Period, Situation, IB.action, 
                 orderId, order.ts, volume, price)
  }
  
  ############################# For Manual Override #############################
  if(nrow(z) == 0 & nrow(x) > 0)
  {
    z <- left_join(x, y, by = c("ticker")) %>% 
          mutate(Situation = "Manual") %>% 
          rename(volume = shares) %>% 
          select(algoId, ticker, DP.Method, MA.Type, Period, Situation, 
                 IB.action, orderId, order.ts, volume, price)  
  }
  
  z <- bind_rows(z, IB.04.activity) %>%
        ungroup() %>% arrange(desc(order.ts)) %>% distinct()
  
  assign("IB.04.activity", z, envir = .GlobalEnv)
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  rm(x, y, z)
  rm(twsExecution, req.Exec.2, req.Exec.df, print.twsExecution, envir = .GlobalEnv)
}

Update.Targets <- function()
{
  if(IB.Parms[["Emergency"]]) {return(cat("\n\nSystem halted due to Emergency"))}
  
  x <- anti_join(IB.01.targets,
                 IB.04.activity %>% distinct() %>% 
                   group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
                   filter(order.ts == max(order.ts)) %>%
                   mutate(ds = as.Date(order.ts)) %>%
                   select(ticker, algoId, DP.Method, MA.Type, Period, ds),
                 by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period", "ds")) 
  
  assign("IB.01.targets", x, envir = .GlobalEnv)
  rm(x)
}
