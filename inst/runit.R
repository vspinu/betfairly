library(RCurl)
library(XML)
setwd("~/Dropbox/works/betfair/betfair/R/")
source("betfair.R")
source("classes.R")
source("funcs.R")
load("../../sysdata.rda")
options(bfCurlOpts =
        list(## cainfo = "c:\\pkg\\R\\R-2.13\\library\\RCurl\\CurlSSL\\cacert.pem",
             ssl.verifypeer = FALSE))


bfLogin("vitoshka", "vitamit178")
bfSessionToken()
print(keepAlive())
bfLogout()
bfSessionToken()

#### GET EVENTS
bfLogin("vitoshka", "vitamit178")
tdf <- getAllEventTypes()
head(tdf)
getAllEventTypes(out = "s4")
getActiveEventTypes()
class(getActiveEventTypes(output = "S4"))
getActiveEventTypes(out = "list")
getActiveEventTypes()

class(getEvents(out = "S4"))
getEvents()

#### GET MARKETS
options(bfServer = "AU")
df <- getAllMarkets(7, server = "AU")
head(df)
options(bfServer = "UK")


df <- getAllMarkets(1, c("GBR"))
head(df)
df <- head(subset(df, marketName == "Match Odds"))
mt <- df$marketID[[5]]
getMarket("102902130", out = "s4")
getMarket("102902130")
length(getMarket("102902130")@runners) == 0L
getMarket(mt)
## any event id from hierarchy
## getEvents(2022802)

getMarketInfo("102759800")
getMarketInfo(mt)
getMarketInfo(mt, output = "S4")

(sid <- getMarket(mt)@runners$selectionId[[1]])
getMarketTradedVolume(mt, sid, output = "simple")
getMarketTradedVolume(mt, sid, output = "S4")

getMarketTradedVolumeCompressed(mt)

#### GET PRICES
getCompleteMarketPricesCompressed(mt, out = "s4")
getCompleteMarketPricesCompressed(mt)

getMarketPrices(mt, out = "s4")
getMarketPrices(mt)

getMarketPricesCompressed(mt, out = "xml")
getMarketPricesCompressed(mt, out = "S4")
getMarketPricesCompressed(mt)

#### PLACE BETS
df <- getAllMarkets(1, from = Sys.time(), to = Sys.Date() + 1)
str(df)
str(getAllMarkets(1))
df1 <- df[sample(nrow(df), 1), ]
mid <- df1$marketID
(m <- getMarket(mid))

getMarketPricesCompressed(mid)

bt <- bfBet(mid, m@runners$selectionId[[1]], 100, 2)
(out <- placeBets(bt))
cancelBets(out$betId)
cancelBets("15838821475")

(out1 <- placeBets(, mid, m@runners$selectionId, c(500, 200), sizes = 2))
cancelBets(out1$betId)
cancelBetsByMarket(mid)

## bt <- bfBetUpdate("15837289001", newPrice = 50, oldPrice = 50, newSize = 4, oldSize = 2)
## updateBets(, out1$betId, newPrices = c(100, 200), oldPrices = c(100, 200), newSize = 5, oldSizes = 2)
bt <- out1$betId[[2]]
getBet(bt, out = "xml")
getBet("15811859239", out = "xml")
getBetLite("15811859239", out = "xml")
getBetLite(out1$betId[[2]])
getBetMatchesLite("15811859239")
out <- getBetHistory(, c(1, 7))
class(Sys.time())
getMUBets(betStatus = "U", matchedSince=Sys.Date())
getMUBets(mid)




#### ACCOUNT MANAGEMENT
getAccountFunds()
getAccountStatement(out = "xml")
getSubscriptionInfo()
## transferFunds(2)

viewProfile()
getMarketProfitAndLoss(mid)

