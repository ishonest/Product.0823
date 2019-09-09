# WARNING: Start TWS Before Proceeding
source("./Functions/20190823.T01.Start.R")

# -------------------------------------------------------------------------
# WARNING: Restarting System: All logs will be deleted
# Do Not Use Unless Necessary
# -------------------------------------------------------------------------
# source("./Functions/20190823.T00.Restart.R")
# -------------------------------------------------------------------------
while(lubridate::hour(format(Sys.time(), tz = "US/Eastern")) < IB.Parms[["Stop.Trading.At"]])
{
  View(IB.01.targets)
  if(IB.Parms[["System.Live"]])
  {
    IB.Account.Status()
    
    IB.Actions()
    View(IB.02.actions)    
    IB.Order()
    IB.Action.Plots()
  }
  
  IB.Next.Run()
  
  IB.Cancel.Orders()
  Sys.sleep(10)
  Update.Activity()
  View(IB.04.activity)
}

# -------------------------------------------------------------------------
# End of Day Process
# -------------------------------------------------------------------------
source("./Functions/20190823.T06.CloseDay.R")

# -------------------------------------------------------------------------
# WARNING: Emergency Process: All positions will be closed
# -------------------------------------------------------------------------
# source("./Functions/20190823.T07.Emergency.R")

