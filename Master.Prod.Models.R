# -------------------------------------------------------------------------
# Initialization
# -------------------------------------------------------------------------
source("./Functions/20190823.Functions.R")

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

if(file.exists(paste0(Parms$data.folder, "Trading/02.Historical.Activity.rds")))
{
  in.hand <- readRDS(paste0(Parms$data.folder, "Trading/02.Historical.Activity.rds")) %>%
              group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
              summarise(volume = sum(ifelse(IB.action == "BUY", volume, -volume))) %>%
              filter(!is.na(algoId))

  # Selecting Models in buyalgos or having a position
  prod.models <- prod.models %>% arrange(ticker, ID, R, algoId) %>%
                  left_join(in.hand, by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
                  filter(algoId %in% Parms$buyalgos | !is.na(volume)) %>%
                  select(-volume)
  rm(in.hand)
}

hist.d1 <- readRDS(paste0(Parms$data.folder, "Summary/Clean.Prices.rds")) %>% 
            filter(ticker %in% unique(prod.models$ticker))

# -------------------------------------------------------------------------
# Incremental Data Pull
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
        , .multicombine = TRUE, .inorder = FALSE, .errorhandling = 'remove'
) %dopar%
{
  # ticker <- unique(prod.models$ticker)[1]
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
source("./Functions/20190823.Trading.Simulation.R")

if(file.exists(paste0(Parms$data.folder, "Trading/06.Historical.Misses.rds")))
{
  hist.miss <- readRDS(paste0(Parms$data.folder, "Trading/06.Historical.Misses.rds")) %>% 
                select(-c(volume, Type, m.price, tickerID, Exchange)) %>%
                rename(missed = action)
  
  if(nrow(hist.miss) == 0)
  {
    hist.miss <- data.frame(  ticker = character(), ds = as.Date(character())
                            , missed = character(), algoId = character(), ID = integer()
                            , DP.Method = character(), MA.Type = character(), Period = numeric()
                            , stringsAsFactors = FALSE )
  }
} else
{
  hist.miss <- data.frame(  ticker = character(), ds = as.Date(character())
                          , missed = character(), algoId = character(), ID = integer()
                          , DP.Method = character(), MA.Type = character(), Period = numeric()
                          , stringsAsFactors = FALSE )
}

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
  # ticker = "CBMG"
  d1 <- all.d1 %>% filter(ticker == !!ticker) %>% ungroup() %>%
        select(ds, volume, open, low, high, close) %>% arrange(ds) %>% 
        mutate(ROI.l = -zoo::rollmax(-low, 5, fill = NA, align = "left")/lag(close)
               , ROI.h = zoo::rollmax(high, 5, fill = NA, align = "left")/lag(close) )
  
  all <- readRDS(paste0(Parms$data.folder, "Scores/", ticker, ".rds"))
  all <- left_join(all, d1, by = "ds") %>% mutate(Score = round(Score, 4))
  
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
          left_join(hist.miss, by = c("ds", "ticker", "algoId", "ID", "DP.Method", "MA.Type", "Period"))

  sim <- foreach(algo.ID = Parms$algoIds, .combine = bind_rows, .errorhandling = 'remove') %:%
          foreach(model.ID = unique(all$ID), .combine = bind_rows, .errorhandling = 'remove') %do%
  {
    # algo.ID = Parms$algoIds[1]
    # model.ID = 168
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
  can.buy <- sim %>% group_by(ticker, ds) %>%
              summarise(BOD.in.hand = sum(!is.na(action))) %>% ungroup() %>% 
              filter(ds == max(ds)) %>% select(BOD.in.hand) %>% as.numeric()
  can.buy <- floor(max(Parms$invest.max.ticker/Parms$invest.max.model - can.buy, 0))

  suppressWarnings
  {
    sim <- sim %>% 
            # # Select today's targets
            filter(ds == max(ds)) %>%
            filter(!is.na(buy.price) | !is.na(sell.price)) %>%
            # # Filter within capacity only
            group_by(action, Type) %>% 
            mutate(rank = case_when(Type == "LONG" ~ rank(-buy.price, ties.method = "min"),
                                    Type == "SHRT" ~ rank(buy.price, ties.method = "min"))) %>%
            filter(!is.na(action) | rank <= can.buy) %>%
            # # Filter HOLD or Can be Bought
            filter(!is.na(buy.price) | grepl("HOLD", action)) %>%
            # # Select the series of these candidates
            ungroup() %>% select(algoId, ID) %>% inner_join(sim, by = c("algoId", "ID")) %>%
            # # Assign buying limits based on yesterday's trade volumes
            group_by(algoId, ID) %>% arrange(ID, ds) %>%
            mutate(invest = case_when(!is.na(buy.price) & !grepl("HOLD|SELL", action)
                                      ~ round(Parms$max.capacity*lag(volume*(open + close)/2), 0))
                   , invest = pmin(invest, Parms$invest.max.model)) %>% 
            ungroup() 
  }
  
  if(is.null(sim) || nrow(sim) == 0)
  {
    saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
    rm(ticker, d1, all, sim, can.buy)
    return(data.frame())
  } else
  {
    today <- sim %>% group_by(ticker, ID, algoId) %>% filter(ds == max(ds)) %>%
              select(algoId, ticker, ds, Type, buy.price, sell.price, stop.price, last.sell, action, invest, 
                     ID, DP.Method, MA.Type, Period)
    
    saveRDS(sim, paste0(Parms$data.folder, "Simulation/", ticker, ".rds"))
    saveRDS(ticker, paste0(Parms$data.folder, "Process.Tracker/", ticker, ".rds"))
    rm(ticker, d1, all, sim, can.buy)
    return(today)
  }
  
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

