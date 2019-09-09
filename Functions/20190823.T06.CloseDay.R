IB.FinishDay <- function(Force.Close = FALSE)
{
  NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
  if(!(NY.Time > 16.00 | Force.Close))
  {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
  rm(NY.Time)

  IB.Account.Status()
  IB.Cancel.Orders()
  Update.Activity()

  assign("IB.04.activity", IB.04.activity, envir = .GlobalEnv)
  
  View(IB.04.activity)
  View(IB.03.orders)

  h.activity <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds")) %>%
                bind_rows(IB.04.activity) %>% distinct() %>% arrange(desc(order.ts))

  h.latest <- h.activity %>%
              group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
              summarise(units = sum(units, na.rm = TRUE)) %>%
              filter(units != 0)

  h.orders <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds")) %>%
              bind_rows(IB.03.orders) %>% distinct() %>% arrange(desc(order.ts))

  saveRDS(h.latest, paste0(IB.Parms[["data.folder"]], "Trading/00.Latest.rds"))
  saveRDS(h.activity, paste0(IB.Parms[["data.folder"]], "Trading/02.Historical.Activity.rds"))
  saveRDS(h.orders, paste0(IB.Parms[["data.folder"]], "Trading/03.Historical.Orders.rds"))

  do.call(file.remove, list(list.files(paste0(IB.Parms$data.folder, "Simulation/"), full.names = TRUE)))
  rm(h.activity, h.orders, h.latest, Force.Close)
  rm(list = setdiff(ls(envir = .GlobalEnv), c("tws")), envir = .GlobalEnv)
}

IB.FinishDay()

twsDisconnect(tws)
rm(list = ls())
gc()