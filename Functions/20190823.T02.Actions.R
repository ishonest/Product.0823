IB.Actions <- function(today = Sys.Date())
{
  # today <- Sys.Date()
  if(length(unique(IB.01.targets$ticker)) == 0) 
  {return(cat("\nNo Targets Available. Check Today's Targets. \n"))}
  
  # Pull Latest Data
  tickers <- readRDS(paste0(IB.Parms[["data.folder"]], "Trading/01.Targets.rds")) %>%
              select(ticker) %>% distinct() %>% unlist()
  
  d1 <- foreach(ticker = tickers, .packages = "BatchGetSymbols"
                , .combine = bind_rows, .errorhandling = 'remove') %do%
        {
          # ticker = unique(IB.01.targets$ticker)[1]
          d1 <- get.clean.data(ticker, src = "yahoo",
                               first.date = today, last.date  = today + 1) %>%
            rename(ds = ref.date, open = price.open, high = price.high, 
                   low = price.low, close = price.close) %>%
            select(ticker, ds, open, high, low, close)
          
          rm(ticker)
          return(d1)
        }
  
  rm(tickers)
  
  # Report Data Pull Errors
  failed <- setdiff(unique(IB.01.targets$ticker), unique(d1$ticker))
  if(length(failed) > 0)
  {cat("\nData Pull Failed for", length(failed), "targets:", paste(failed, collapse = ", ") )}
  rm(failed)
  
  d1 <- d1 %>% mutate(NY.Time = as.numeric(format(Sys.time(), tz = "US/Eastern", format = "%H.%M")))
  
  if(nrow(d1) == 0)
  {
    rm(d1)
    return(cat("\nNo Data Found. Check Connection with Yahoo!!!"))
  }
  
  # Summary of Current Investment
  if(nrow(IB.00.positions) > 0)
  {
    x <- IB.00.positions %>% select(symbol, position, averageCost) %>%
          rename(ticker = symbol, cost = averageCost) %>%
          left_join(d1 %>% select(ticker, close), by = "ticker") %>%
          mutate(Investment = abs(position*cost), 
                 Position = abs(position*close),
                 Gain = position*(close - cost)) %>%
          summarise(Investment = sum(Investment, na.rm = TRUE),
                    Position = sum(Position, na.rm = TRUE),
                    ROI = round(100*sum(Gain, na.rm = TRUE)/Investment, 2) )
    
    cat(paste0("\n: Invested   : $ ", formatC(x$Investment, format="f", big.mark=",", digits=0),
               "\n: Position   : $ ", formatC(x$Position, format="f", big.mark=",", digits=0),
               "\n: ROI        :   ", paste0(x$ROI, "%"),
               "\n-------------------------------\n"))
    
    rm(x)

  }
  
  # Eligible Orders: Based on current price only
  o1 <- inner_join(IB.01.targets, d1, by = c("ticker", "ds")) %>%
        mutate(action = case_when(Type == "LONG" & invest > 0 & 
                                    close <= buy.price & close > stop.price ~ "BUY"
                                  , Type == "LONG" & action == "HOLD" & 
                                    sell.price <= high ~ "SELL"
                                  , Type == "LONG" & action == "HOLD" & 
                                    close <= stop.price ~ "STOP SELL"
                                  , Type == "LONG" & action == "HOLD" & 
                                    (!is.na(last.sell) & last.sell >= 0) &
                                    between(NY.Time, IB.Parms[["Last.Sell.At"]], 16) ~ "EOD SELL"
                                  
                                  , Type == "SHRT" & invest > 0 & 
                                    close >= buy.price & close < stop.price ~ "BUY"
                                  , Type == "SHRT" & action == "HOLD" & 
                                    sell.price >= low ~ "SELL"
                                  , Type == "SHRT" & action == "HOLD" & 
                                    close >= stop.price ~ "STOP SELL"
                                  , Type == "SHRT" & action == "HOLD" & 
                                    (!is.na(last.sell) & last.sell >= 0) &
                                    between(NY.Time, IB.Parms[["Last.Sell.At"]], 16) ~ "EOD SELL"
        )) %>%
        filter(!is.na(action))
  
  # Get Current Positions
  position <- IB.04.activity %>% 
              group_by(ticker, algoId, DP.Method, MA.Type, Period) %>%
              summarise(model.position = 1.0*sum(ifelse(IB.action == "BUY", volume, -volume))) %>%
              filter(model.position > 0) %>%
              left_join(IB.00.positions %>% rename(ticker = symbol) %>%
                          group_by(ticker) %>% summarise(ticker.position = sum(position))
                        , by = "ticker") %>%
              filter(ticker.position >= model.position) %>% ungroup()
  

  # Integrate Current Position w/ Eligible Orders
  # Filter out Sell orders w/ no positions
  # Create IB Specifications
  o2 <- left_join(o1, position, 
                 by = c("ticker", "algoId", "DP.Method", "MA.Type", "Period")) %>%
        filter(grepl("BUY", action) | (grepl("SELL", action) & model.position > 0)) %>%
        mutate(t.price = case_when(action %in% c("BUY", "SELL") ~ round(close, 2))
               , m.price = case_when(action == "SELL" ~ sell.price,
                                     action == "STOP SELL" ~ stop.price,
                                     # NA if "EOD SELL"
                                     action == "BUY" ~ buy.price )
               , volume = case_when(grepl("SELL", action) ~ model.position,
                                    grepl("BUY", action) ~ floor(invest/t.price))
               , IB.orderType = case_when(action %in% c("BUY", "SELL") ~ "LMT",
                                          TRUE ~ "MKT")
               , IB.action = case_when(Type == "LONG" ~ action,
                                    Type == "SHRT" & grepl("BUY", action) ~ "SELL",
                                    Type == "SHRT" & grepl("SELL", action) ~ "BUY")
               ) %>%
        select(ticker, ds, Type, action, IB.action, IB.orderType, volume, t.price, m.price, 
               algoId, ID, DP.Method, MA.Type, Period, Exchange, tickerID, NY.Time)
  
  # Order within fund limits: Check if we need the cummin function
  o3 <- o2 %>%
        group_by(action) %>%
        mutate(Cost = if_else(action == "BUY", -volume*t.price, volume*t.price)
               , Available.Funds = Available.Funds + cumsum(Cost)
        ) %>%
        filter(Available.Funds > 0 | action != "BUY") %>% 
        select(-c(Cost, Available.Funds)) %>%
        ungroup()
  
  assign("IB.02.actions", o3, envir = .GlobalEnv)
  rm(d1, o1, o2, o3, position, today)
  gc()
}

IB.Action.Plots <- function()
{
  if(nrow(IB.02.actions) == 0) return(cat("\n"))

  for(i in 1:nrow(IB.02.actions))
  {
    # i = 1
    x <- IB.02.actions[i, ]
    df <- readRDS(paste0(IB.Parms[["data.folder"]], "Simulation/", x$ticker, ".rds")) %>%
          semi_join(x, by = c("algoId", "ID")) %>%
          mutate(  A.Score = ifelse(!is.na(buy.price), Score, NA)
                   , P.Score = ifelse(is.na(buy.price), Score, NA)
                   , T.Score = ifelse(!is.na(A.Score*lead(P.Score)) |
                                        !is.na(lag(A.Score)*P.Score) |
                                        !is.na(P.Score*lead(A.Score)) |
                                        !is.na(lag(P.Score)*A.Score)
                                      , Score, NA)
          ) %>%
          filter(ds > IB.Parms[["last.dev.date"]])

    x <- IB.02.actions[i,] %>%
        mutate(buy.price = last(df$buy.price)
               , sell.price = last(df$sell.price)
               , stop.price = last(df$stop.price)
               , Model.ID = paste(algoId, DP.Method, MA.Type, Period)) %>%
        select(-c(tickerID, algoId, DP.Method, MA.Type, Period, m.price)) %>%
        bind_cols(df %>% filter(!is.na(ROI)) %>%
                    summarise(N = n()
                              , ROI = paste0(round(100*mean(ROI), 1), "%")
                              , invest.period = round(mean(invest.period), 2)) )

    if(max(df$close, na.rm = TRUE) > 20)
    {
      min.y <- 10*floor(min(df$close/10, na.rm = TRUE)/1.25) # 1.25 to accomodate buy/sell prices
      max.y <- 10*ceiling(max(df$close/10, na.rm = TRUE)*1.25)
    } else
    {
      min.y <- floor(min(df$close, na.rm = TRUE)/1.25)
      max.y <- ceiling(max(df$close, na.rm = TRUE)*1.25)
    }

    t1 <- paste(x$ticker, x$Model.ID, x$Type, sep = " | ")
    t2 <- ifelse(is.na(x$t.price),
                 paste0(x$action, " ", x$volume, " Units @ Market Price"),
                 paste0(x$action, " ", x$volume, " Units @ $", round(x$t.price, 2)))
    
    t3 <- paste0("Buy $", x$buy.price, " | Sell $", x$sell.price, " | Stop Loss $", x$stop.price)
    t4 <- ifelse(x$N <= 0, "No History"
                 , paste0("History: ", x$N, " times for ", x$invest.period, " Days | ROI ", x$ROI))

    p <- plot_ly(data = df, x = ~ds) %>%
      add_trace(y = ~close, name = "Close",
                type = 'scatter', mode = 'lines',
                line = list(color = '#a6a6a6', width = 0.5),
                fill = 'tozeroy', fillcolor='#f9f7f7' ) %>%
      add_trace(y = ~P.Score, name = 'Passive Score', yaxis = "y2",
                type = 'scatter', mode = 'lines',
                line = list(color = '#3f6ea6', dash='dot', width = 1.5) ) %>%
      add_trace(y = ~A.Score, name = 'Active Score', yaxis = "y2",
                type = 'scatter', mode = 'lines',
                line = list(color = '#3f6ea6', width = 1.5) ) %>%
      add_trace(y = ~T.Score, name = 'Transition Score', yaxis = "y2",
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#cc0000', dash='dot', width = 1) ) %>%
      add_trace(y = ~buy.price, name = 'Buy Price', type = 'scatter', mode = 'lines',
                line = list(color = '#444444', width = 1) ) %>%
      add_trace(y = ~sell.price, name = 'Sell Price', type = 'scatter', mode = 'lines',
                line = list(color = '#444444', width = 1) ) %>%

      add_annotations(text = t1, xref = 'paper', yref = 'paper', x = 0, y = 1.09,
                      font = list(size = 14, color = '#004080'), showarrow = F) %>%
      add_annotations(text = t2, xref = 'paper', yref = 'paper', x = 0, y = 1.05,
                      font = list(size = 12, color = '#004080'), showarrow = F) %>%
      add_annotations(text = paste0("New York Time: ", x$NY.Time),
                      xref = 'paper', yref = 'paper', x = 1, y = 1.09,
                      font = list(size = 12), showarrow = F) %>%
      add_annotations(text = t3, xref = 'paper', yref = 'paper', x = 1, y = 1.06,
                      font = list(size = 12, color = '#004080'), showarrow = F) %>%
      add_annotations(text = t4, xref = 'paper', yref = 'paper', x = 1, y = 1.03,
                      font = list(size = 12, color = '#004080'), showarrow = F) %>%

      layout( font = list(size = 12), showlegend = FALSE, hovermode = 'compare',
              margin = list(l = 70, r = 70, b = 50, t = 50, pad = 10),
              xaxis = list(title = NA, showgrid = FALSE),
              yaxis = list(title = "Close ($)", color = '#a6a6a6',
                           range = c(min.y, max.y), showgrid = FALSE),
              yaxis2 = list(title = "Score", color = '#3f6ea6', tickformat = ".1%",
                            overlaying = "y", side = "right", showgrid = TRUE) )

    fname <- paste0(IB.Parms[["data.folder"]], "Plots/", x$ticker, " ", x$Model.ID, ".html")

    saveWidget(as_widget(p)
               , title = paste0(x$ticker, ": ", x$Type, " ", x$action, " "
                                , x$volume, " Units @ $", x$t.price)
               , libdir = "libdir"
               , file = file.path(normalizePath(dirname(fname)),basename(fname))
               )

    browseURL(fname)

    rm(max.y, min.y, t1, t2, t3, t4, fname, p, i, df, x)

  }

}
