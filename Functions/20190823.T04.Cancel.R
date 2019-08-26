IB.Cancel.Orders <- function()
{
  AF.Open.Orders <- function(return.df = FALSE)
  {
    Open.Orders <- function(tws)
    {
      .reqOpenOrders(tws)
      con <- tws[[1]]
      eW  <- eWrapper()
      socketSelect(list(con), FALSE, NULL)
      curMsg <- readBin(con, character(), 1L)
      processMsg(curMsg, con, eW)
    }
    
    open <- data.frame()
    i <- 0 # Counter to extract information between 2 OPEN_ORDER_END messages
    n <- 0 # Counter to control the max messages / secton
    while(i < 2)
    {
      x <- Open.Orders(tws)
      if(!is.null(x) && !is.list(x))
      {
        #  5: .twsIncomingMSG$OPEN_ORDER
        # 53: .twsIncomingMSG$OPEN_ORDER_END
        if(x[1] == 53) {i = i + 1} else 
          if(x[1] == 5) {open <- bind_rows(open, data.frame(t(x), stringsAsFactors = FALSE))}
      }
      
      rm(x)
      n <- n + 1
      if(n %% 40 == 0) {Sys.sleep(1)}
    }
    
    rm(i, n, Open.Orders)
    
    if(nrow(open) > 0)
    {
      open <- open %>% distinct() %>%
        rename(IB.Version = X2, orderId = X3, conId = X4, symbol = X5, sectype = X6, 
               strike = X10, currency = X11, action = X13, totalQuantity = X14,
               orderType = X15, lmtPrice = X16, auxPrice = X17, tif = X18, 
               outsideRTH = X19, account = X20, orderRef = X23, parentId = X25
        ) %>%
        select(account, parentId, orderId, conId, symbol, sectype, strike, currency,
               action, totalQuantity, orderType, lmtPrice, auxPrice, tif, IB.Version) %>%
        mutate(orderId = as.integer(orderId)
               , parentId = as.integer(parentId)
               , totalQuantity = as.numeric(totalQuantity)
               , lmtPrice = as.numeric(lmtPrice)
               , auxPrice = as.numeric(auxPrice) )
    } else 
    {
      open <- data.frame(account = character(), parentId = integer(), orderId = integer()
                         , conId = character(), symbol = character(), sectype = character()
                         , strike = character(), currency = character(), action = character()
                         , totalQuantity = numeric(), orderType = character()
                         , lmtPrice = numeric(), auxPrice = numeric(), tif = character()
                         , IB.Version = character(), stringsAsFactors = FALSE)
      
      cat("\nThere are NO Open Orders ... ")
    }
    
    assign("IB.05.open.orders", open, envir = .GlobalEnv)
    
    if(return.df == TRUE) 
    {return(open)} else
    {rm(open)}
  }

  IB.05.open.orders <- AF.Open.Orders(return.df = TRUE)
  
  if(exists("IB.05.open.orders", envir = .GlobalEnv) & nrow(IB.05.open.orders) > 0)
  {
    to.be.cancelled <- IB.05.open.orders
    
    if(nrow(to.be.cancelled) > 0)
    {
      IB.03.orders <- to.be.cancelled %>% select(-IB.Version) %>% distinct() %>% 
                      mutate(order.ts = Sys.time(), order.type = "Order Cancelled") %>%
                      bind_rows(IB.03.orders) %>%
                      arrange(desc(order.ts))
      
      assign("IB.03.orders",  IB.03.orders, envir = .GlobalEnv)
      
      for(i in 1:nrow(to.be.cancelled))
      {cancelOrder(twsconn = tws, orderId = to.be.cancelled$orderId[i])}
      rm(i)
    }
    
    rm(to.be.cancelled)
  }
  
  rm(AF.Open.Orders)
}
