
bfLogin("username", "password")
bfSessionToken()
print(keepAlive())
bfLogout()
bfSessionToken()

#### GET EVENTS
head(getAllEventTypes())
getActiveEventTypes()
getActiveEventTypes(out = "list")

getEvents()

#### GET MARKETS

head(getAllMarkets(1))
head(getAllMarkets(7, server = "AU"))
options(bfServer = "AU")
head(getAllMarkets(7))
options(bfServer = "UK")


df <- getAllMarkets(1, c("GBR"))
head(df)
(df <- head(subset(df, marketName == "Match Odds")))
(mt <- df$marketID[[5]])

getMarket("102902130")
length(getMarket("102902130")@runners) == 0L
getMarket(mt)
## any event id from hierarchy
getEvents(2022802)

getMarketInfo("102759800")
getMarketInfo(mt)
getMarketInfo(mt, output = "S4")

(sid <- getMarket(mt)@runners$selectionId[[1]])
# one selection only
getMarketTradedVolume(mt, sid)
# full market
getMarketTradedVolumeCompressed(mt)

#### GET PRICES
## simple output is not implemented
getCompleteMarketPricesCompressed(mt, out = "xml")

getMarketPrices(mt)

getMarketPricesCompressed(mt, out = "xml")
getMarketPricesCompressed(mt, out = "S4")
getMarketPricesCompressed(mt)

#### PLACE BETS
df <- getAllMarkets(1, from = Sys.time(), to = Sys.Date() + 1)
head(df)
df1 <- df[sample(nrow(df), 1), ]
mid <- df1$marketID
(m <- getMarket(mid))

getMarketPricesCompressed(mid)

## create a list of bets and pass as fist argument to placeBets

## betfair will automatically  match for the best price if your price is lower
## than the market price.
sIds <- m@runners$selectionId
bt1 <- bfBet(mid, sIds[[1]], price = 100, size = 2)
bt2 <- bfBet(mid, sIds[[2]], price = 100, size = 3)

(out <- placeBets(list(bt1, bt2)))
cancelBets(out$betId)

## One bet works as well:
(out <- placeBets(bt1))
cancelBets(out$betId)


## some arbitrary not existent bet bet
cancelBets("1583882147555")

## alternatively you can specify the arguments in mapplyish way. Arguments are
## recycled if needed:
(out <- placeBets(, mid, sIds, c(500, 200), sizes = 2))
## cancelBets(out$betId)

## non free API:
cancelBetsByMarket(mid)

## bt <- bfBetUpdate("15837289001", newPrice = 50, oldPrice = 50, newSize = 4, oldSize = 2)
## updateBets(, out1$betId, newPrices = c(100, 200), oldPrices = c(100, 200), newSize = 5, oldSizes = 2)
bt <- out$betId[[2]]
getBet(bt)
getBet("15811859239")
getBetLite(out1$betId[[2]])
getBetMatchesLite("15811859239")
(out <- getBetHistory(, c(1, 7)))
getMUBets(betStatus = "U", matchedSince=Sys.Date())
getMUBets(mid)

getMarketProfitAndLoss(mid)


#### ACCOUNT MANAGEMENT
getAccountFunds()
getAccountFunds("AU")
getAccountStatement(startDate = Sys.Date()-4)
getSubscriptionInfo()

## should do this only if registered for Australian markets,  otherwise will not be able to get funds back:
## transferFunds(2)
viewProfile()


#### S4 functionality
bfInitClasses()
getAllEventTypes(out = "s4")
class(getActiveEventTypes(output = "S4"))
class(getEvents(out = "S4"))
getMarket("102902130", out = "s4")
getMarketTradedVolume(mt, sid, output = "S4")
getCompleteMarketPricesCompressed(mt, out = "s4")
getMarketPrices(mt, out = "s4")
getBetLite("15811859239", out = "xml")
getAccountStatement(out = "xml")
