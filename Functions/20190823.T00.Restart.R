IB.Restart <- function()
{
  IB.03.orders <- data.frame(account         = character()
                             , parentId      = integer()
                             , orderId       = integer()
                             , conId         = character()
                             , algoId        = character()
                             , symbol        = character()
                             , sectype       = character()
                             , strike        = character()
                             , currency      = character()
                             , action        = character()
                             , totalQuantity = numeric()
                             , orderType     = character()
                             , orderStatus   = character()
                             , lmtPrice      = numeric()
                             , auxPrice      = numeric()
                             , tif           = character()
                             , order.ts      = as.POSIXct(character(), tz = Sys.timezone())
                             , order.type    = character()
                             , stringsAsFactors=FALSE)
  
  IB.04.activity <- data.frame(ticker = character()
                               , algoId = character()
                               , Type = character()
                               , DP.Method = character()
                               , MA.Type = character()
                               , Period = numeric()
                               , Situation = character()
                               , IB.action = character()
                               , order.ts = as.POSIXct(character(), tz = Sys.timezone())
                               , volume = numeric()
                               , price = numeric()
                               , stringsAsFactors=FALSE)
  
  saveRDS(IB.04.activity, paste0(IB.Parms[["data.folder"]], "/Trading/02.Historical.Activity.rds"))
  saveRDS(IB.03.orders, paste0(IB.Parms[["data.folder"]], "/Trading/03.Historical.Orders.rds"))

  assign("IB.03.orders", IB.03.orders, envir = .GlobalEnv)
  assign("IB.04.activity", IB.04.activity, envir = .GlobalEnv)
  
  IB.Parms[["Last.Order.Time"]] <- Sys.time()
  assign("IB.Parms", IB.Parms, envir = .GlobalEnv)

  rm(IB.04.activity, IB.03.orders)
  
  IB.Account.Status()
}

### Close All positions before restarting
source("./Functions/20190823.T07.Emergency.R")

IB.Restart()
cat("\nSystem has been reset \n")
rm(IB.Restart)