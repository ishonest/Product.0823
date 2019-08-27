

x <- data.frame(ticker = "AAPL", Exchange = "NASDAQ", algoId = "20190823",
                IB.action = "BUY", volume = 1, IB.orderType = "MKT", t.price = 200,
                stringsAsFactors = FALSE
                )
i = 1

Contract <- twsEquity(symbol = x$ticker[i], local = x$ticker[i], primary = x$Exchange[i]
                      , currency = "USD", exch = "SMART")

Order <- twsOrder(orderId = reqIds(tws)
                  , orderRef = x$algoId[i]
                  , action = x$IB.action[i]
                  , clientId = tws$clientId
                  , account = IB.Parms[["acctCode"]]
                  , totalQuantity = x$volume[i]
                  , orderType = x$IB.orderType[i]
                  , lmtPrice = x$t.price[i]
                  , tif = "GTC")


IBrokers::placeOrder(twsconn = tws, Contract, Order)
IB.05.open.orders <- AF.Open.Orders(return.df = TRUE)
AF.Update.Orderbook(Contract, Order, order.type = "Order Placed")



