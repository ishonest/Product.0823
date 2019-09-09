# Fix this
Update.Activity <- function(Last.Order = IB.Parms[["Last.Order.Time"]])
{
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
          filter(order.ts > Last.Order - 10) %>%
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
    return(cat("\nThere were NO Recent Activities ... "))
  }
  
  
  # For Normal or Emergency Executions
  x1 <- inner_join(IB.02.actions, x, by = c("IB.action", "ticker")) %>%
          mutate(Situation = ifelse(IB.Parms[["Emergency"]] == TRUE, "Emergency", "Normal")) %>%
          group_by(ticker, IB.action) %>% 
          arrange(ticker, IB.action, DP.Method, MA.Type, Period) %>%
          # select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, IB.action, 
          #        order.ts, price, volume, volume.executed) %>%
          mutate(vol.cum = cumsum(volume),
                 vol.left = ifelse(volume.executed >= vol.cum, 0, vol.cum - volume.executed),
                 vol.left = pmin(volume, vol.left),
                 vol.real = volume - vol.left
                 ) %>%
          select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, IB.action, 
                 order.ts, vol.real, price) %>%
          rename(volume = vol.real)
    
  # For Manual Overrides
  x2 <- IB.04.activity %>%
        group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
        summarise(volume = sum(volume, na.rm = TRUE)) %>%
        filter(volume != 0) %>%
        mutate(Type = ifelse(volume > 0, "LONG", "SHRT"),
               IB.action = ifelse(volume > 0, "SELL", "BUY"), 
               volume = abs(volume), 
               Situation = "Manual") %>%
        inner_join(x, by = c("IB.action", "ticker")) %>%
        group_by(ticker, IB.action) %>%
        arrange(ticker, IB.action, DP.Method, MA.Type, Period) %>%
        mutate(vol.cum = cumsum(volume),
               vol.left = ifelse(volume.executed >= vol.cum, 0, vol.cum - volume.executed),
               vol.left = pmin(volume, vol.left),
               vol.real = volume - vol.left
        ) %>%
        select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, IB.action, 
               order.ts, vol.real, price) %>%
        rename(volume = vol.real) %>%
        filter(volume > 0)
  
  if(exists("IB.02.actions", envir = .GlobalEnv) && nrow(IB.02.actions) > 0)
  {
    x2 <- anti_join(x2, IB.02.actions %>% select(ticker, DP.Method, MA.Type, Period) %>% distinct(),
                    by = c("ticker", "DP.Method", "MA.Type", "Period"))
  }
  
  x <- bind_rows(x1, x2) %>% ungroup()
  rm(x1, x2)

  
  # Update Targets
  y <- x %>% 
        mutate(units.changed = ifelse(IB.action == "BUY", volume, -volume)) %>%
        select(ticker, algoId, DP.Method, MA.Type, Period, units.changed) %>%
        full_join(IB.01.targets, by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
        mutate(units.changed = ifelse(is.na(units.changed), 0, units.changed),
               units = units - units.changed) %>%
        filter(units != 0) %>%
        select(-units.changed)

  x <- bind_rows(x, IB.04.activity) %>% ungroup() %>% arrange(desc(order.ts)) %>% distinct()
  assign("IB.04.activity", x, envir = .GlobalEnv)
  assign("IB.01.targets", y, envir = .GlobalEnv)
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  rm(x, y)
}

Correct.Activity <- function(First.Order = as.POSIXct(paste(Sys.Date(), "09:30:00"), tz = "America/New_York"))
{
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
        left_join(x %>% filter(order.ts >= First.Order - 10) %>% select(-c(price, order.ts))
                  , by = c("ticker", "IB.action")) %>%
        group_by(ticker, IB.action) %>% 
        arrange(ticker, IB.action, DP.Method, MA.Type, Period) %>%
        mutate(Situation = "Corrected",
               vol.cum = cumsum(volume),
               vol.left = ifelse(volume.executed >= vol.cum, 0, vol.cum - volume.executed),
               vol.left = pmin(volume, vol.left),
               vol.real = volume - vol.left
        ) %>%
        select(ticker, algoId, Type, DP.Method, MA.Type, Period, Situation, IB.action, 
               order.ts, vol.real, price) %>%
        rename(volume = vol.real) %>% filter(volume > 0) %>%
        mutate(volume = ifelse(IB.action == "BUY", volume, -volume)) %>%
        bind_rows(IB.04.activity %>% filter(order.ts < First.Order - 10))
  
  assign("IB.04.activity", x1, envir = .GlobalEnv)
  rm(x1, x, First.Order)

}

