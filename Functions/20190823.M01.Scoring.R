# Swapped the formula of close and adjusted 7/May
# Use adjusted (i.e. adjusted) for ROI.c

rm(list = ls())
gc()
options(scipen = 3)
set.seed(1024)

library(dplyr)
library(doSNOW)
library(foreach)

closeAllConnections()
cl <- makeCluster(4, outfile="dopar_log.txt")
registerDoSNOW(cl)

# -------------------------------------------------------------------------
# Associated Functions
# -------------------------------------------------------------------------
Get.MA.Compressed <- function(d1, periods = seq(10, 180, 10))
{
  library(TTR)
  library(foreach)
  library(dplyr)
  
  d1[is.na(d1)] <- 0
  
  suppressWarnings(
    z <- foreach(n = periods, .combine = bind_rows, .errorhandling = 'remove') %:%
      foreach(name = c("open", "low", "high", "close")
              , .combine = bind_rows, .errorhandling = 'remove') %do%
              {
                z <- d1 %>%
                  mutate(Period = n,
                         MA.Var = name,
                         SMA := lag(SMA(get(name), n))/lag(close),
                         EMA := lag(EMA(get(name), n))/lag(close),
                         EMAW := lag(EMA(get(name), n, wilder = TRUE))/lag(close),
                         ZLEMA := lag(ZLEMA(get(name), n))/lag(close),
                         HMA := lag(HMA(get(name), n))/lag(close)
                  ) %>%
                  select(ds, ticker, Period, MA.Var, SMA, EMA, EMAW, ZLEMA, HMA) %>%
                  na.omit()
                
                rm(n, name)
                return(z)
              }
  )
  
  return(z)
}

AF.LN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>% filter(Period == !!n) %>%
    select(ds, ticker, MA.Var, (!!MA.Type)) %>%
    rename(MA = MA.Type)
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())}
  
  d2x <- d2x %>%
    mutate(LN = log(MA)) %>%
    select(-MA) %>% data.frame() %>%
    reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
    left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}

AF.MALN <- function(d2, d1x, MA.Type, n)
{
  d2x <- d2 %>%
    filter(Period == !!n) %>%
    select(ds, ticker, MA.Var, (!!MA.Type)) %>%
    rename(MA = MA.Type)
  
  if(min(d2x$MA) <= 0 ){ return(data.frame())}
  
  d2x <- d2x %>%
    mutate(LN = log(MA)) %>%
    data.frame() %>%
    reshape(idvar = c("ds", "ticker"), timevar = "MA.Var", direction = "wide") %>%
    left_join(d1x, by = c("ticker", "ds"))
  
  return(d2x)
  
}
# -------------------------------------------------------------------------
# Batch Clean Data Functions
# -------------------------------------------------------------------------
Get.Data.Clean <- function(d1, min.trade = 100000, min.tdays = 250, bad.jumps = 0.1, in.portfolio = FALSE)
{
  library(dplyr)
  library(zoo)
  library(timeDate)
  
  # NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  # if(NY.Time >= 9.3) {d1 <- d1 %>% filter(ref.date < Sys.Date())}
  # rm(NY.Time)
  
  if(in.portfolio == TRUE)
  {
    min.trade <- 0
    min.tdays <- 0 
    bad.jumps <- 0.001
  }
  rm(in.portfolio)
  
  source("./Functions/20190823.M00.Trading.Days.R")
  
  d1 <- d1 %>% distinct()
  if(nrow(d1) < min.tdays) 
  {
    cat(ticker, ": Data Pull from Yahoo Failed\n")
    rm(min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }
  
  # ~100 Days of Prod + 150 Days of Dev: 250 Days of Good Data
  # Definition of Good Data: No Extreme Jumps, Non-NA/Zero Volume, $250k+ Daily Trading
  # Consistent for 250 Days
  
  # Volume Check
  d2x <- d1 %>% group_by(ticker) %>%
    mutate(volume = na_if(volume, 0)) %>%
    summarise(N = n(),
              last.na = suppressWarnings(max(which(is.na(volume)))),
              last.na = ifelse(is.infinite(last.na), 0, last.na),
              good.days = N - last.na
    ) %>%
    # filter(good.days >= min.tdays) %>%
    select(-c(N, good.days)) %>%
    inner_join(d1, by = "ticker") %>%
    group_by(ticker) %>%
    arrange(ticker, ref.date) %>%
    filter(row_number() > last.na) %>%
    rename(ds = ref.date, adjusted = price.adjusted,
           open = price.open, high = price.high,  low = price.low, close = price.close) %>%
    select(c(ds, ticker, volume, open, high, low, close, adjusted)) %>%
    # mutate_all(zoo::na.locf) # Replace NA with preceeding values
    mutate_at(vars(-group_cols()), zoo::na.locf) # Effective from dplyr 0.8.3
  
  if(nrow(d2x) < min.tdays) 
  {
    cat(d1$ticker[1], ": Filtered @ Cleaning -", nrow(d2x), "days with Valid Volume\n")
    rm(d2x, min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }
  
  # Remove data until with low value trades & sudden jumps
  d2y <- d2x %>%
    group_by(ticker) %>%
    arrange(ticker, ds) %>%
    mutate(value = adjusted*volume,
           delta = lead(adjusted)/adjusted,
           bad = case_when(delta < bad.jumps ~ TRUE,
                           delta > (1/bad.jumps) ~ TRUE,
                           value < min.trade ~ TRUE,
                           TRUE ~ FALSE)) %>%
    summarise(N = n(),
              last.bad = suppressWarnings(max(which(bad == TRUE))),
              last.bad = ifelse(is.infinite(last.bad), 0, last.bad),
              good.days = N - last.bad ) %>%
    select(-c(N, good.days))
  
  d1 <- inner_join(d2x, d2y, by = "ticker") %>%
    group_by(ticker) %>%
    arrange(ticker, ds) %>%
    filter(row_number() > last.bad) %>%
    select(-last.bad) %>%
    ungroup()
  
  if(nrow(d1) < min.tdays) 
  {
    cat("Filtered @ Cleaning [[", ticker, "]]:",
        paste0(nrow(d1), " days with $", round(min.trade/1000, 2), "k in trade and"),
        paste0("price swings within [", bad.jumps*100, "%, ", 100/bad.jumps, "%]\n"))
    
    rm(d2x, d2y, min.trade, min.tdays, bad.jumps)
    rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
    return(data.frame())
  }
  
  # -------------------------------------------------------------------------
  d1 <- bind_rows(d1,  data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                  ticker = unique(d1$ticker), stringsAsFactors = FALSE)) %>%
    group_by(ticker) %>% arrange(ticker, ds) %>%
    mutate(ds.N = row_number())
  
  rm(d2x, d2y, min.trade, min.tdays, bad.jumps)
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  gc()
  
  return(d1)
  
}

# -------------------------------------------------------------------------
# Baseline Model Scores Function
# -------------------------------------------------------------------------
Get.Model.Scores <- function(ticker, d1,
                             T.models = NULL, # Pass NULL for initialization,
                             in.days = 5L,
                             Type = "Development",
                             last.dev.date = as.Date("2019-01-31"),
                             data.folder = "./Data/")
{
  # -------------------------------------------------------------------------
  # Inputs: For Testing
  # -------------------------------------------------------------------------
  # library(foreach)
  # library(dplyr)
  # # all.d1 <- readRDS(paste0(data.folder, "IB/Clean.Prices.rds"))
  # ticker <- prod.models$ticker[1]
  # d1 <- all.d1 %>% filter(ticker == !!ticker)
  # T.models <- prod.models %>% filter(ticker == !!ticker) %>%
  #             select(DP.Method, MA.Type, Period, ID) %>% distinct()
  # in.days = 5
  # Type = "Production"
  # last.dev.date = as.Date("2019-01-31")
  # data.folder = "F:/Project S/MA Linear Modelling/"

  # -------------------------------------------------------------------------
  # Raw Data Extraction
  # -------------------------------------------------------------------------
  d1x <- d1 %>% mutate(last.close = lag(close)
                       , ROI.h = zoo::rollmax(high, in.days, fill = NA, align = "left")/last.close
                       , ROI.l = -zoo::rollmax(-low, in.days, fill = NA, align = "left")/last.close
                       , ROI.c = lead(adjusted, in.days-1)/lag(adjusted)
                       , Bare = as.factor(ifelse(ROI.c > 1, 1, 0))
                       ) %>%
          select(-c(adjusted, last.close, ROI.c))

  d2 <- Get.MA.Compressed(d1, periods = seq(10, 180, 10))

  if(is.null(T.models))
  {
    T.models <- expand.grid(Period = unique(d2$Period),
                            MA.Type = c("SMA", "EMA", "EMAW", "ZLEMA", "HMA"),
                            DP.Method = c("LN", "MALN"),
                            stringsAsFactors = FALSE) %>%
                mutate(ID = row_number())
  }

  # -------------------------------------------------------------------------
  # Trend Models
  # -------------------------------------------------------------------------
  # profvis::profvis({

  T.scores <- data.frame()
  T.specs <- data.frame()

  for(i in T.models$ID)
  {
    # i = 10
    DP.Method <- T.models$DP.Method[T.models$ID == i]
    MA.Type <- T.models$MA.Type[T.models$ID == i]
    n <- T.models$Period[T.models$ID == i]
    d2x <- get(paste0("AF.", DP.Method))(d2, d1x, MA.Type, n)

    if(nrow(d2x) == 0)
    {
      rm(i, MA.Type, n, DP.Method, d2x)
      next()
    }

    # -------------------------------------------------------------
    tdata <- d2x %>% filter(ds <= last.dev.date) %>%
      select(-c("ds", "ds.N", "ticker", "ROI.h", "ROI.l", "volume", "open", "high", "low", "close"))

    m2 <- tryCatch(glm(Bare ~ ., family = "binomial", data = tdata) %>%
                     MASS::stepAIC(direction = "both", trace = FALSE)
                   , error = function(w) {return(NULL)}
                   , warning = function(w) {return(NULL)})

    # -------------------------------------------------------------
    # Checking Validity of Models
    # -------------------------------------------------------------
    if(is.null(m2) || grepl("LN.", paste(names(m2$coefficients), collapse = ", ")) == FALSE)
    {
      rm(i, MA.Type, n, DP.Method, d2x, tdata, m2)
      next()
    }

    all <- d2x %>% arrange(ds.N) %>%
            mutate(ID = i, DP.Method, Period = n, MA.Type,
                   Score = predict(m2, newdata = ., type = "response")) %>%
            select(ticker, ds, ID, DP.Method, Period, MA.Type, Score)

    model.specs <- data.frame(t(m2$coefficients))
    rownames(model.specs) <- NULL
    model.specs <- data.frame(ticker, ID = i, DP.Method, Period = n, MA.Type, stringsAsFactors = FALSE) %>%
      bind_cols(model.specs)
    colnames(model.specs)[grepl("Intercept", names(model.specs))] <- "Intercept"

    T.scores <- bind_rows(T.scores, all)
    T.specs <- bind_rows(T.specs, model.specs)

    rm(i, MA.Type, n, DP.Method, d2x, tdata, m2, all, model.specs)
  }

  # })  # For Profvis
  # -------------------------------------------------------------------------
  # Summarization, Saving and Cleaning
  # -------------------------------------------------------------------------
  if(Type == "Production") 
  {
    saveRDS(T.scores, paste0(data.folder, "Scores/", ticker, ".rds"))
  } else if(nrow(T.scores) > 0) # For Development
  {
    # saveRDS(T.scores, paste0(data.folder, "Development/Scores/", ticker, ".rds"))
    # saveRDS(T.specs, paste0(data.folder, "Development/Specs/", ticker, ".rds"))
  }

  rm(d1, d1x, d2, T.models, in.days, Type, last.dev.date, ticker, T.scores, T.specs)
  gc()

}

# -------------------------------------------------------------------------
# Incremental Data Pull
# -------------------------------------------------------------------------
Get.Incremental.Data <- function(stocks, first.date)
{
  all.d1 <- data.frame()
  
  while(length(stocks) > 0)
  {
    d1 <- foreach(ticker = stocks, .combine = bind_rows
                  , .errorhandling = 'remove', .packages = c("BatchGetSymbols", "timeDate") ) %dopar%
          {
            source("./Functions/20190823.M00.Trading.Days.R")
            # ticker = stocks[1]
            d1 <- BatchGetSymbols::get.clean.data(ticker, src = "yahoo", 
                                                  first.date, last.date = Sys.Date() ) %>%
                  rename(ds = ref.date, open = price.open, high = price.high, low = price.low,
                         close = price.close, adjusted = price.adjusted) %>%
                  arrange(ds) %>%
                  select(ds, ticker, volume, open, high, low, close, adjusted)
            
            if(max(d1$ds) != PrevTradingDate() | any(is.na(d1)))
            {
              rm(d1, ticker, NextTradingDate, PrevTradingDate, TradingDates)
              return(NULL)
            }
            
            d1 <- d1 %>% bind_rows(data.frame(ds = NextTradingDate(Date = max(d1$ds)),
                                              ticker, stringsAsFactors = FALSE))
            
            rm(ticker, NextTradingDate, PrevTradingDate, TradingDates)
            return(d1)
          }
    
    if(is.null(d1) || nrow(d1) == 0) 
    {
      cat("\nData Pull/Clean failed for", length(stocks), "stocks\n", stocks, "...\n")
      rm(d1, stocks)
      break
    } else 
    {
      all.d1 <- hist.d1 %>% na.omit() %>% select(-ds.N) %>% filter(ticker %in% unique(d1$ticker)) %>%
                bind_rows(d1) %>%
                group_by(ticker) %>% arrange(ticker, ds) %>% mutate(ds.N = row_number()) %>%
                bind_rows(all.d1) %>% distinct()
      stocks <- setdiff(stocks,  unique(d1$ticker))
      rm(d1)
      
      if(length(stocks) == 0) 
      {
        cat("\nData Pull 100% Succcessful.\n\n")
        rm(stocks, first.date)
        break
      }
      
      cat("\nTrying Again for:", length(stocks), "stocks:", stocks, "...\n\n")
    }
  }
  
  all.d1 <- all.d1 %>% ungroup()
  return(all.d1)
  
}
