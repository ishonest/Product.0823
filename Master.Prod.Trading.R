
# -------------------------------------------------------------------------
# source("./Functions/20190823.T00.Restart.R")
# -------------------------------------------------------------------------
source("./Functions/20190823.T01.Start.R")

while(lubridate::hour(format(Sys.time(), tz = "US/Eastern")) < IB.Parms[["Stop.Trading.At"]])
{
  View(IB.01.targets)

  if(IB.Parms[["System.Live"]])
  {
    IB.Cancel.Orders()
    Sys.sleep(10)
    
    Update.Activity()
    Update.Targets()
    View(IB.04.activity)
    
    IB.Account.Status()
    
    IB.Actions()
    IB.Order()
  }
  
  cat("\nCycle Complete: Next cycle will commence at", format(Sys.time() + 120, "%X"), "...\n")
  Sys.sleep(120)

}

# -------------------------------------------------------------------------
IB.FinishDay(Force.Close = TRUE)
twsDisconnect(tws)
rm(list = ls())

# -------------------------------------------------------------------------
# Emergency
# -------------------------------------------------------------------------
source("./Functions/20190823.T07.Emergency.R")

