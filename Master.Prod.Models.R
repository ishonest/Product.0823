# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
source("./Functions/20190823.M01.Scoring.R")

Parms <- list(algoIds = c("20190823")
              , buyalgos = c("20190823")
              , invest.max.model  = 1000   # Maximum investment in a model
              , invest.max.ticker = 3000   # Maximum investment in a ticker
              , max.capacity      = 0.01   # Max % of yesterday's volume any model can buy
              , data.folder       = "./Data/"
              , last.dev.date     = as.Date("2019-01-31")
              )

prod.models <- data.frame()
for (algoId in Parms$algoIds)
{
  prod.models <- bind_rows(prod.models, readRDS(paste0(Parms$data.folder, "Summary/", 
                                                       algoId, ".Production.Models.rds")))
  rm(algoId)
}

# Selecting Models in buyalgos or having a position
if(file.exists(paste0(Parms$data.folder, "Trading/00.Latest.rds")))
{
  in.hand <- readRDS(paste0(Parms$data.folder, "Trading/00.Latest.rds"))

  prod.models <- prod.models %>% arrange(ticker, ID, R, algoId) %>%
                  left_join(in.hand, by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
                  filter(algoId %in% Parms$buyalgos | !is.na(volume)) %>%
                  select(-volume)
}

hist.d1 <- readRDS(paste0(Parms$data.folder, "Summary/Clean.Prices.rds")) %>% 
            filter(ticker %in% unique(prod.models$ticker))

# -------------------------------------------------------------------------
# Incremental Data Pull: Works in the middle of the trading
# -------------------------------------------------------------------------
all.d1 <- Get.Incremental.Data(stocks = unique(hist.d1$ticker),
                               first.date = max(hist.d1$ds))

rm(hist.d1)
# -------------------------------------------------------------------------
# Rescoring with New Data
# -------------------------------------------------------------------------
do.call(file.remove,
        list(list.files(paste0(Parms$data.folder, c("Process.Tracker/", "Scores/")), full.names = TRUE)))

stocks <- setdiff(unique(all.d1$ticker),
                  gsub(".rds", "", list.files(paste0(Parms$data.folder, "Process.Tracker/"))) )

foreach(ticker = stocks
        , .export = c("AF.LN", "AF.MALN"), .packages = c("dplyr", "foreach")
        , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove' ) %dopar%
        {
          # ticker <- "ODT"
          d1 <- all.d1 %>% filter(ticker == !!ticker)
          T.models <- prod.models %>% filter(ticker == !!ticker) %>%
                        select(DP.Method, MA.Type, Period, ID) %>% distinct()
            
          Get.Model.Scores(ticker, d1, T.models, Type = "Production")
          saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
          
          rm(ticker, d1, T.models)
        }

rm(list = lsf.str())
rm(stocks)
# -------------------------------------------------------------------------
# Get Targets: Stocks in active zone + price points 
# Filters investment in more than 3 models
# Assigns the investment value
# Keeps record of the holding investment for sell
# -------------------------------------------------------------------------
source("./Functions/20190823.M02.Simulation.R")

do.call(file.remove,
        list(list.files(paste0(Parms$data.folder, c("Process.Tracker/", "Simulation/"))
                        , full.names = TRUE)))

stocks <- setdiff(gsub(".rds", "", list.files(paste0(Parms$data.folder, "Scores/"))), 
                  gsub(".rds", "", list.files(paste0(Parms$data.folder, "Process.Tracker/")))
                  )

targets <- foreach(ticker = stocks, .combine = bind_rows, .packages = c("dplyr", "foreach"),
                   .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
                   ) %dopar%
{
  # ticker = "PTI"
  targets <- Get.Simulation(ticker)
  return(targets)
}

overview <- readRDS(paste0(Parms$data.folder, "Summary/Overview.rds")) %>%
            select(ticker, Exchange, tickerID) %>%
            distinct()

targets <- left_join(targets, overview, by = "ticker") %>%
            mutate(Exchange = ifelse(Exchange == "NASDAQ", "ISLAND", Exchange))

saveRDS(targets, paste0(Parms$data.folder, "Trading/01.Targets.rds"))

# -------------------------------------------------------------------------
do.call(file.remove, list(list.files(paste0(Parms$data.folder, c("Process.Tracker/", "Scores/")), full.names = TRUE)))

stopCluster(cl)
rm(list = ls())
gc()
