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
                               , orderId = integer()
                               , order.ts = as.POSIXct(character(), tz = Sys.timezone())
                               , volume = numeric()
                               , price = numeric()
                               , stringsAsFactors=FALSE)
  
  IB.06.missed.orders <- data.frame()
  
  saveRDS(IB.04.activity, paste0(IB.Parms[["data.folder"]], "/Trading/02.Historical.Activity.rds"))
  saveRDS(IB.03.orders, paste0(IB.Parms[["data.folder"]], "/Trading/03.Historical.Orders.rds"))
  saveRDS(IB.06.missed.orders, paste0(IB.Parms[["data.folder"]], "/Trading/06.Historical.Misses.rds"))
  
  rm(IB.04.activity, IB.03.orders, IB.06.missed.orders)
}



### Close All positions before restarting
source("./Functions/20190823.T07.Emergency.R")

IB.Restart()
cat("\nSystem has been reset \n")
rm(IB.Restart)