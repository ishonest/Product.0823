# Settings: https://blog.quantinsti.com/ibpy-tutorial-implement-python-interactive-brokers-api/
# Initialization inside IB.StartDay

# -------------------------------------------------------------------------
# Global Parameter Calls
# -------------------------------------------------------------------------
IB.Parms <- list(  last.dev.date    = as.Date("2019-01-31")
                   , data.folder      = "./Data/"
                   
                   , clientId         = 100           # Must be a positive integer
                   , acctCode         = "DU1617055"   # IB Account Number
                   
                   , invest.max       = 25000         # Maximum position
                   , invest.min       = 500           # Minimum investment in a model
                   , Start.Trading.At = 9.35          # NY Time to start buying
                   , Stop.Trading.At  = 16            # NY Time to stop trading
                   , Last.Sell.At     = 15.50         # NY Time to initiate EOD Sell
                   
                   , Emergency        = FALSE
                   , System.Live      = FALSE
                   , Last.Order.Time  = Sys.time()
                   )

# -------------------------------------------------------------------------
rm(list = setdiff(ls(envir = .GlobalEnv), c("IB.Parms")), envir = .GlobalEnv)
closeAllConnections()
options(scipen = 3, digits = 8, digits.secs = 0)
set.seed(1024)
library(IBrokers)
library(dplyr)
library(foreach)
library(BatchGetSymbols)
library(plotly)
library(htmlwidgets)
library(IBrokers)
# -------------------------------------------------------------------------
# Start-Up Functions 
# -------------------------------------------------------------------------
IB.Account.Status <- function()
{
  AF.Connect <- function()
  {
    if(!exists("tws", envir = .GlobalEnv) || tws$clientId != IB.Parms[["clientId"]])
    {
      closeAllConnections()
      if(exists("tws", envir = .GlobalEnv)) {rm(tws, envir = .GlobalEnv)}
      tws <- list(clientId = -1)
      # This loop makes sure that the client id is what supplied
      while(tws$clientId != IB.Parms[["clientId"]] )
      {
        tws <- tryCatch({twsConnect(clientId = IB.Parms[["clientId"]])}
                        , warning = function(w) {list(clientId = -1)}
                        , error = function(e) {list(clientId = -2)} )
      }
      
      assign("tws", tws, envir = .GlobalEnv)
      cat("\nConnection was Re-established\n")
      print(tws)
    }

    setServerLogLevel(tws)
  }
  
  AF.Connect()

  cancelAccountUpdates(conn = tws)
  d <- reqAccountUpdates(conn = tws, acctCode = IB.Parms[["acctCode"]], subscribe = TRUE)

  assign("IB.00.account",
         data.frame(t(sapply(d[[1]],c)), stringsAsFactors = FALSE), 
         envir = .GlobalEnv)
  
  d2 <- d[[2]]
  IB.00.positions <- data.frame()
  if(length(d2) > 0)
  {
    for(i in 1:length(d2))
    {
      x <- bind_cols(data.frame(t(sapply(d2[[i]][1][[1]],c)), stringsAsFactors = FALSE),
                     data.frame(t(sapply(d2[[i]][2][[1]],c)), stringsAsFactors = FALSE) )
      
      IB.00.positions <- bind_rows(IB.00.positions, x)
      rm(x, i)
    }
    
    IB.00.positions <- IB.00.positions[c("accountName", "conId", "symbol", "position",
                                   "marketPrice", "marketValue", "averageCost",
                                   "unrealizedPNL", "realizedPNL",
                                   "local", "currency", "exch", "primary", "sectype",
                                   "expiry", "strike", "right",
                                   "multiplier", "combo_legs_desc", "comboleg",
                                   "include_expired", "secIdType", "secId")]
    
    cols.to.numeric <- c("strike", "right", "position", "marketPrice",
                         "marketValue", "averageCost", "unrealizedPNL")
    IB.00.positions[cols.to.numeric] <- as.numeric(as.matrix(IB.00.positions[cols.to.numeric]))
    assign("IB.00.positions", IB.00.positions, envir = .GlobalEnv)
    rm(cols.to.numeric)
  }
  IB.00.positions <- IB.00.positions %>% filter(position != 0)
  assign("IB.00.positions", IB.00.positions, envir = .GlobalEnv)
  
  if(nrow(IB.00.positions) > 0)
  {
    x <- IB.00.positions %>% ungroup() %>%
          summarise(Current.Position = sum(abs(marketValue), na.rm = TRUE),
                    Investment = sum(abs(position*averageCost), na.rm = TRUE),
                    Available.Funds = IB.Parms[["invest.max"]] - Investment,
                    ROI.investment = ifelse(Investment > 0, 
                                            round(100*(Current.Position/Investment - 1), 2), 0),
                    ROI.portfolio = sum(as.numeric(unrealizedPNL), as.numeric(realizedPNL)),
                    ROI.portfolio = round(100*ROI.portfolio/IB.Parms[["invest.max"]], 2))
    
    assign("Available.Funds", x$Available.Funds, envir = .GlobalEnv)
    cat(paste0("\n------------------------------------------------", 
               "\n: Fund ROI   :   ", paste0(x$ROI.portfolio, "%"),
               "\n: Available  : $ ", formatC(x$Available.Funds, format="f", big.mark=",", digits=0),
               "\n------------------------------------------------\n"  ))
    
    if(x$Available.Funds <= 1000) {cat("\nWarning!!! Available Funds running low.\n")}
    rm(x)
    
  } else
  {
    assign("Available.Funds", IB.Parms[["invest.max"]], envir = .GlobalEnv)
    cat("\nThere have been no trades on this account yet!") 
  }

  rm(d, d2, AF.Connect)
  
}

IB.System.Status <- function()
{
  source("./Functions/20190823.M00.Trading.Days.R")
  NY.Time <- round(as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M")), 2)
  Next.Day <- format(NextTradingDate(), '%A, %B %d, %Y')
  Trade.Days <- TradingDates()
  rm(NextTradingDate, PrevTradingDate, TradingDates, envir = .GlobalEnv)
  
  # -------------------------------------------------------------------------
  Time.Difference <- function(to, fr)
  {
    # to = 9.3
    # fr = NY.Time
    to <- strptime(sprintf("%.2f", to), format="%H.%M")
    fr <- strptime(sprintf("%.2f", fr), format="%H.%M") 
    
    d <- difftime(to, fr, units = "hours")
    d.hours <- floor(as.numeric(d))
    d.minutes <- round((as.numeric(d)%% 1 * 60))
    
    op <- if(d.hours == 0) 
    {
      if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
    } else if(d.hours == 1)
    {
      paste("1 hour and", 
            if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
      )
    } else
    {
      paste(d.hours, "hours and", 
            if(d.minutes == 1) {paste("1 minute")} else {paste(d.minutes, "minutes")}
      )
    }
    
    rm(to, fr, d, d.hours, d.minutes)
    return(op)
  }
  
  # -------------------------------------------------------------------------
  if(!(Sys.Date() %in% Trade.Days)) 
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat(paste("\nNYSE & NASDAQ are closed today. Markets will reopen on", Next.Day, "... \n"))
    rm(NY.Time, Next.Day, Trade.Days)
  } else if(NY.Time < IB.Parms[["Start.Trading.At"]])
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat("\nMarkets are closed now. Will reopen in", Time.Difference(to = 9.3, fr = NY.Time), "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else if(IB.Parms[["Emergency"]] == TRUE)
  {
    IB.Parms[["System.Live"]] <- FALSE
    cat("\nSystem halted due to Emergency", "... \n")
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  } else 
  {
    IB.Parms[["System.Live"]] <- TRUE
    rm(NY.Time, Next.Day, Trade.Days, Time.Difference)
    
  }
  
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
}

IB.StartDay <- function()
{
  # assign("IB.00.Latest", readRDS(paste0(IB.Parms[["data.folder"]], "Trading/00.Latest.rds"))
  #        , envir = .GlobalEnv)
  assign("IB.01.targets"
         , readRDS(paste0(IB.Parms[["data.folder"]], "Trading/01.Targets.rds"))
         , envir = .GlobalEnv)
  assign("IB.03.orders"
         , data.frame(stringsAsFactors = FALSE)
         # , readRDS(paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds"))
         , envir = .GlobalEnv)
  assign("IB.04.activity"
         , readRDS(paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds"))
         , envir = .GlobalEnv)
  
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)
  
  IB.Account.Status()
  IB.System.Status()
}

IB.Next.Run <- function()
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(NY.Time < IB.Parms$Start.Trading.At)
  {
    next.run <- difftime(strptime(paste(Sys.Date(), IB.Parms$Start.Trading.At), "%Y-%m-%d %H.%M", 
                           tz = "US/Eastern"),
                        lubridate::with_tz(Sys.time(), tzone = "US/Eastern"),
                        units = "secs" 
                        ) %>% as.numeric() %>% ceiling()
  } else
  {
    next.run <- 120
  }
  
  cat("\nCycle Complete: Next cycle will commence at", format(Sys.time() + next.run, "%X"), "...\n")
  Sys.sleep(next.run)
  rm(next.run, NY.Time)
  cat("\n--------------------------------------------------------------\n")
  IB.System.Status()
}

IB.StartDay()
rm(IB.StartDay)

# -------------------------------------------------------------------------
# Call Other Trading Functions
# -------------------------------------------------------------------------
source("./Functions/20190823.T02.Actions.R")
source("./Functions/20190823.T03.Order.R")
source("./Functions/20190823.T04.Cancel.R")
source("./Functions/20190823.T05.Activity.R")
