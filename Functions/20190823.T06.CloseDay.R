# IB.Missed.Orders <- function(today = Sys.Date())
# {
#   d1 <- foreach(ticker = unique(IB.01.targets$ticker),
#                 .combine = bind_rows, .errorhandling = 'remove') %do%
#     {
#       d1 <- get.clean.data(ticker, src = "yahoo", first.date = today, last.date  = today + 1) %>%
#         rename(ds = ref.date, open = price.open, high = price.high, 
#                low = price.low, close = price.close) %>%
#         select(ticker, ds, open, high, low, close)
#       
#       rm(ticker)
#       return(d1)
#     }
#   
#   x <- IB.01.targets %>%
#     left_join(d1, by = c("ticker", "ds")) %>%
#     left_join(IB.04.activity %>% 
#                 group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
#                 summarise(in.hand = sum(ifelse(action == "BUY", volume, -volume))),
#               by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
#     mutate(action = case_when(buy.price >= low & invest > 0 ~ "BUY",
#                               sell.price <= high & in.hand > 0 ~ "SELL",
#                               last.sell > 0 & in.hand > 0 ~ "EOD SELL" ),
#            m.price = case_when(action == "BUY" ~ buy.price,
#                                action == "SELL" ~ sell.price,
#                                action == "EOD SELL" ~ last.sell),
#            volume = case_when(action == "BUY" ~ floor(invest/buy.price),
#                               action %in% c("SELL", "EOD SELL") ~ in.hand)
#     ) %>%
#     filter(!is.na(action)) %>%
#     select(ds, ticker, action, volume, m.price, algoId, DP.Method, MA.Type, Period, tickerID, Exchange)
#   
#   assign("IB.06.missed.orders", x, envir = .GlobalEnv)
#   rm(today, d1, x)
#   
# }
# 
# IB.FinishDay <- function(Force.Close = FALSE)
# {
#   NY.Time <- as.numeric(strftime(format(Sys.time(), tz = "US/Eastern"), format = "%H.%M"))
#   if(!(NY.Time > 16.00 | Force.Close))
#   {return(cat("\nWarning!!! Market is Still Open. \nCannot Shutdown the System ... "))}
#   rm(NY.Time)
#   
#   IB.Cancel.Orders()
#   IB.Missed.Orders()
#   
#   h.missed <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/06.Historical.Misses.rds")) %>%
#     bind_rows(IB.06.missed.orders) %>% distinct()
#   
#   h.activity <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/02.Historical.Activity.rds")) %>%
#     bind_rows(IB.04.activity) %>% distinct() %>% arrange(desc(order.ts))
#   
#   h.orders <- readRDS(paste0(IB.Parms[["data.folder"]], "Production/Trading/03.Historical.Orders.rds")) %>%
#     bind_rows(IB.03.orders) %>% distinct() %>% arrange(desc(order.ts))
#   
#   saveRDS(h.missed, paste0(IB.Parms[["data.folder"]], "Production/Trading/06.Historical.Misses.rds"))
#   saveRDS(h.activity, paste0(IB.Parms[["data.folder"]], "Production/Trading/02.Historical.Activity.rds"))
#   saveRDS(h.orders, paste0(IB.Parms[["data.folder"]], "Production/Trading/03.Historical.Orders.rds"))
#   
#   rm(h.missed, h.activity, h.orders)
#   rm(list = setdiff(ls(envir = .GlobalEnv), c("tws")), envir = .GlobalEnv)
# }
