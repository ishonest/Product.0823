AF.roll <- function(df, var, width)
{
  # df = y
  # width = 4
  # var = "buy.window"

  df$op <- NA
  
  for(i in 0:width)
  {df$op <- pmax(lag(df[[var]], i), df$op, na.rm = TRUE)}
  
  df <- df %>% select(-var) %>% rename(!!var := op)
  
  rm(var, width, i)
  return(df)
}

AF.Signal.Strength <- function(window, Type)
{
  # window = x$sell.window
  # Type = x$Type
  N <- length(window)
  strength <- rep(NA, N)
  op <- 0
  for(i in 1:N)
  {
    if(is.na(window[i])) {op <- 0}
    else if(!is.na(Type[i-1]) & !is.na(Type[i]) & Type[i-1] != Type[i]) {op <- 1}
    else {op <- op + 1}
    
    if(op > 0) {strength[i] <- op}
    rm(i)
  }
  
  rm(N, op, window, Type)
  return(strength)
}

# -------------------------------------------------------------------------
# AF.simulate trade for long and short trades
# -------------------------------------------------------------------------
AF.simulate.hedge <- function(df, Process = "Development")
{
  # df = sim
  # Process = "Production"
  df <- df %>% ungroup() %>% 
        arrange(ID, ds) %>%
        mutate(buy.signal = case_when(Type == "LONG" & buy.price >= low ~ TRUE,
                                      Type == "SHRT" & buy.price <= high ~ TRUE,
                                      TRUE ~ FALSE),
               sell.signal = case_when(Type == "LONG" & sell.price <= high ~ TRUE,
                                       Type == "SHRT" & sell.price >= low ~ TRUE,
                                       TRUE ~ FALSE),
               stop.signal = case_when(Type == "LONG" & stop.price >= low ~ TRUE,
                                       Type == "SHRT" & stop.price <= high ~ TRUE,
                                       TRUE ~ FALSE) )
  
  buy.signal <- df$buy.signal
  sell.signal <- df$sell.signal
  stop.signal <- df$stop.signal
  
  clean.x <- function(x) 
  {
    x[is.na(x) | x < 0] <- 0
    return(x)
  }
  
  buy.price <- clean.x(df$buy.price)
  sell.price <- clean.x(df$sell.price)
  stop.price <- clean.x(df$stop.price)
  last.sell <- clean.x(df$last.sell)
  
  cost <- 0
  type <- ""
  holding.days <- 0
  trade.val <- 0
  continue <- 1 # To Prevent Buying After Stoploss in a series
  
  action <- rep(NA, nrow(df))
  capacity <- rep(NA, nrow(df))
  ROI <- rep(NA, nrow(df))
  invest.period <- rep(NA, nrow(df))
  continue.trading <- rep(NA, nrow(df))
  
  for(i in 1:nrow(df))
  {
    if(sell.price[i] == 0) {continue <- 1}
    continue.trading[i] <- continue
    
    # Buy Or Sell. If buy, don't sell (Because we don't know the sequence of low and high)
    # Buy only once in a series
    # Do Not Stop Loss on Purchase Day
    
    if(buy.signal[i] & cost == 0 & continue > 0) 
    {
      action[i] <- "BUY"
      type <- df$Type[i]
      cost <- buy.price[i]
      holding.days <- 1
      trade.val <- round(df$volume[i]*(df$open[i] + df$close[i])/2, 0)
      capacity[i] <- trade.val
      
    } else
      if(sell.signal[i] & cost > 0) # Sell
      {
        action[i] <- "TARGET SELL"
        capacity[i] <- trade.val
        ROI[i] <- case_when(type =="LONG" ~ sell.price[i]/cost,
                            type =="SHRT" ~ 2 - sell.price[i]/cost)
        invest.period[i] <- holding.days + 1
        
        cost <- 0
        type <- ""
        holding.days <- 0
        trade.val <- 0
        
      } else
        if(i < nrow(df) & cost > 0 & stop.signal[i]) # Sell @ stop loss
        {
          action[i] <- "STOP SELL"
          capacity[i] <- trade.val
          ROI[i] <- case_when(type =="LONG" ~ stop.price[i]/cost,
                              type =="SHRT" ~ 2 - stop.price[i]/cost)
          
          invest.period[i] <- holding.days
          
          cost <- 0
          type <- NA
          holding.days <- 0
          trade.val <- 0
          continue <- 0
        } else
          if(i < nrow(df) & cost > 0 & last.sell[i] > 0) # Sell @ Close if still to be sold
          {
            action[i] <- "EOD SELL"
            capacity[i] <- trade.val
            ROI[i] <- last.sell[i]/cost
            ROI[i] <- case_when(type =="LONG" ~ last.sell[i]/cost,
                                type =="SHRT" ~ 2 - last.sell[i]/cost)
            invest.period[i] <- holding.days
            
            cost <- 0
            type <- NA
            holding.days <- 0
            trade.val <- 0
          } else
            if(cost > 0) # Hold
            {
              action[i] <- "HOLD"
              holding.days <- holding.days + 1
              if(trade.val > 0) {capacity[i] <- trade.val}
            }

  }
  
  if(Process == "Development")
  {
    op <- df %>% select(-c(buy.window, sell.window, volume, open, low, high, close, ROI.l, ROI.h,
                           buy.signal, sell.signal, stop.signal))
  } 
  
  if(Process == "Production")
  {
    op <- df %>% select(-c(buy.window, sell.window, R.low, R.high, R.buy, R.sell, R.stop, ROI.l, ROI.h,
                           buy.signal, sell.signal, stop.signal))
  }
  
  op <- bind_cols(op, data.frame(continue.trading, action, capacity, ROI, invest.period
                               , stringsAsFactors = FALSE)) %>% arrange(ds) %>%
        # Provisions for Missed Sell
        mutate(missed.sell = case_when(!is.na(in.hand) & 
                                         max(which(grepl("SELL", action))) > max(which(grepl("BUY", action))) &
                                         row_number() > max(which(grepl("BUY", action))) ~ 1,
                                       TRUE ~ 0)
               , action = ifelse(missed.sell == 1, "MISSED SELL", action)
               , Type = ifelse(missed.sell == 1, zoo::na.locf(Type), Type)
               , signal = ifelse(missed.sell == 1 & is.na(signal), zoo::na.locf(signal), signal)
               , sell.price = ifelse(missed.sell == 1 & is.na(sell.price), zoo::na.locf(sell.price), sell.price)
               , stop.price = ifelse(missed.sell == 1 & is.na(stop.price), zoo::na.locf(stop.price), stop.price)
               , last.sell = ifelse(missed.sell == 1 & row_number() > max(which(!!last.sell > 0)),
                                    last(last.sell[!!last.sell > 0]),
                                    last.sell)
               , continue.trading = ifelse(missed.sell == 1, 1, continue.trading)
               , ROI = ifelse(missed.sell == 1, NA, ROI)
               , invest.period = ifelse(missed.sell == 1, NA, invest.period)
               ) %>%
        select(-missed.sell) %>%
        
        # Provisions for Missed Buy
        mutate(missed.buy = case_when(is.na(in.hand) & 
                                        !is.na(last(action)) & last(action) == "HOLD" &
                                        row_number() >= max(which(grepl("BUY", action))) ~ 1,
                                      TRUE ~ 0
                                      )
               , action = ifelse(missed.buy == 1, "MISSED BUY", action)
               , capacity = ifelse(missed.buy == 1, NA, capacity)
               ) %>% 
        select(-missed.buy) %>%
        
        # Removing Redundant Price Points
        mutate(buy.price = case_when(continue.trading > 0 ~ buy.price)
               , sell.price = case_when(continue.trading > 0 ~ sell.price)
               , stop.price = case_when(continue.trading > 0 ~ stop.price)
               , last.sell = case_when(continue.trading > 0 ~ last.sell)
        ) %>%
        select(-continue.trading) %>%
        
        # Provisions for Last Day
        mutate(action = case_when(row_number() == n() & is.na(action) & !is.na(buy.price) & is.na(in.hand) ~ "CAN BUY", 
                                  row_number() == n() & !is.na(action) & action == "HOLD" ~ "CAN SELL",
                                  TRUE ~ action))
  
  rm(buy.signal, sell.signal, stop.signal, buy.price, sell.price, stop.price, last.sell
     , cost, type, holding.days, trade.val, continue
     , action, capacity, ROI, invest.period, continue.trading, df, i, clean.x, Process)
  gc()
  
  return(op)
}

# -------------------------------------------------------------------------
AF.simulate.20190823 <- function(sim)
{
  # sim <- all %>% filter(ID == model.ID, algoId == algo.ID) 
  
  if(nrow(sim) == 0) {return(data.frame())}
  
  sim <- sim %>%
          group_by(ID) %>% arrange(ID, ds) %>%
          mutate(buy.window = case_when(R.buy > 0 ~ 1)) %>%
          AF.roll(df = ., var = "buy.window", width = 3) %>%
          mutate(  sell.window = ifelse(!is.na(lag(buy.window)) | !is.na(buy.window), 1, NA)
                   , Type = zoo::na.locf(Type, na.rm = FALSE)
                   , Type = case_when(sell.window == 1 ~ Type)
                   , signal = AF.Signal.Strength(window = sell.window, Type)
                   
                   , buy.price  = round(R.buy*lag(close), 2)
                   , buy.price  = buy.window*zoo::na.locf(buy.price, na.rm = FALSE)
                   
                   , sell.price = round(R.sell*lag(close), 2)
                   , sell.price = sell.window*zoo::na.locf(sell.price, na.rm = FALSE)
                   
                   # Prorated Stop Loss:  95% - 99% in Day 1-5
                   , stop.price = ifelse(Type == "LONG", 
                                         buy.price*(1 - R.stop), 
                                         buy.price*(1 + R.stop))
                   , stop.price = sell.window*zoo::na.locf(stop.price, na.rm = FALSE)
                   , stop.price = case_when(Type == "LONG" & signal <= 5 ~ (0.94 + signal*0.01)*stop.price,
                                            Type == "SHRT" & signal <= 5 ~ (1.06 - signal*0.01)*stop.price,
                                            signal >  5 ~ stop.price)
                   , stop.price = round(stop.price, 2)
                   
                   , last.sell = case_when(sell.window == 1 & is.na(buy.window) & !is.na(close) ~ close,
                                           sell.window == 1 & is.na(buy.window) & is.na(close) ~ 0)
          ) %>%
          AF.simulate.hedge(., Process = "Production") 
  
  return(sim)
}

# -------------------------------------------------------------------------
Get.Simulation <- function(ticker)
{
  # ticker = "ADMA"
  d1 <- all.d1 %>% filter(ticker == !!ticker) %>% ungroup() %>%
        select(ds, volume, open, low, high, close) %>% arrange(ds) %>% 
        mutate(ROI.l = -zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close)
               , ROI.h = zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close) )
  
  all <- readRDS(paste0(Parms$data.folder, "Scores/", ticker, ".rds")) %>%
          left_join(d1, by = "ds") %>% 
          mutate(Score = round(Score, 4))
  
  if(is.null(all) || nrow(all) == 0)
  {
    saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
    rm(ticker, d1, all)
    next()
  }
  
  all <- all %>% group_by(ID) %>%
        filter(ds <= Parms$last.dev.date) %>%
        mutate(R = ntile(Score, 20)) %>%
        group_by(ID, R) %>%
        summarise(R.low = min(Score)) %>%
        mutate(R.low = ifelse(R == min(R), 0, R.low),
               R.high = ifelse(R == max(R), 1, lead(R.low) - 0.0001)) %>% # works for round 4
        full_join(all, by = "ID") %>%
        filter(Score >= R.low, Score <= R.high) %>% select(-c(R.low, R.high)) %>%
        ungroup() %>% arrange(ticker, ID, ds) %>%
        full_join(data.frame(ticker, algoId = Parms$algoIds, stringsAsFactors = FALSE)
                  , by = "ticker") %>%
        left_join(prod.models
                  , by = c("algoId" , "ticker", "ID", "DP.Method", "MA.Type", "Period", "R")) %>%
        group_by(ticker, ID, algoId) %>%
        filter(!all(is.na(R.low))) %>%
        left_join(in.hand %>% rename(in.hand = units),
                  by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) 

  sim <- foreach(algo.ID = Parms$algoIds, .combine = bind_rows, .errorhandling = 'remove') %:%
          foreach(model.ID = unique(all$ID), .combine = bind_rows, .errorhandling = 'remove') %do%
          {
            # algo.ID = Parms$algoIds[1]
            # model.ID = 49
            sim <- all %>% filter(ID == model.ID, algoId == algo.ID) 
            
            if(algo.ID == "20190823" & nrow(sim) > 0)
            {sim <- AF.simulate.20190823(sim)} else 
            {sim <- data.frame()}
            
            rm(model.ID, algo.ID)
            return(sim)
          }
  
  # Filters investment in more than 3 models
  # Assigns the investment value
  # Keeps record of the holding investment for sell
  bought <- in.hand %>% filter(ticker == !!ticker) %>% nrow()
  can.buy <- floor(max(Parms$invest.max.ticker/Parms$invest.max.model - bought, 0))
  rm(bought)
  
  suppressWarnings
  {
    today <- sim %>% 
            mutate(units = case_when(grepl("SELL", action) ~ (-1)*in.hand,
                                     grepl("BUY", action) & Type == "LONG" 
                                     ~ floor(pmin(Parms$max.capacity*lag(volume), Parms$invest.max.model/buy.price)),
                                     grepl("BUY", action) & Type == "SHRT" 
                                     ~ -floor(pmin(Parms$max.capacity*lag(volume), Parms$invest.max.model/buy.price))
                                     )) %>%
            filter(ds == max(ds) & !is.na(action)) %>%
            # # Push Missed Buys for 3 days max
            filter((action == "MISSED BUY" & signal <= 3) | action != "MISSED BUY") %>%
            # # Filter within capacity only
            mutate(action2 = ifelse(action == "MISSED BUY", NA, action)) %>%
            group_by(action2, Type) %>% 
            mutate(rank = case_when(Type == "LONG" ~ rank(-buy.price, ties.method = "last"),
                                    Type == "SHRT" ~ rank(buy.price, ties.method = "last"))
                   ) %>%
            filter(!is.na(action2) | rank <= can.buy) %>%
            ungroup() %>% select(-action2) %>%
            select(algoId, ticker, ds, buy.price, sell.price, stop.price, last.sell, signal, 
                   action, units, ID, DP.Method, MA.Type, Period) %>%
            rename(active.day = signal)
  }
  
  if(is.null(today) || nrow(today) == 0)
  {
    today <- data.frame()
  } else
  {
    sim <- today %>% ungroup() %>% select(algoId, ID) %>% 
            inner_join(sim, by = c("algoId", "ID")) %>%
            group_by(algoId, ID) %>% arrange(ID, ds) %>% ungroup() %>%
            select(c(ticker, ds, ID, Type, signal, action, 
                     Score, volume, open, low, high, close, buy.price, sell.price, stop.price, last.sell, 
                     capacity, ROI, invest.period, algoId, DP.Method, MA.Type, Period, R)) %>% 
            rename(active.day = signal)
    
    saveRDS(sim, paste0(Parms$data.folder, "Simulation/", ticker, ".rds"))
  }    
  
  saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
  rm(ticker, d1, all, sim, can.buy)
  return(today)
  
}
