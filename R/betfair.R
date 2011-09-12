##' The \code{betfairly} package allows to access most of the  Betfair
##' \href{https://docs.developer.betfair.com/betfair/}{API} directly from R.
##'
##' For the list of all implemented functions and the details of the current
##' development status please see
##' \href{https://code.google.com/p/betfairly/source/browse/trunk/inst/extra/todo.org}{todo}.
##'
##' If a particular functionality that you need is missing,  please contact the author.
##'
##' For examples of usage see \href{https://code.google.com/p/betfairly/source/browse/trunk/inst/extra/examples.R}{here}.
##'
##' \bold{Table of most common functions:}
##'
##' \tabular{ll}{
##' \bold{If you want to:} \tab \bold{Use:}\cr
##' Login \tab \code{\link{bfLogin}}\cr
##' Request a list of available events \tab \code{\link{getActiveEventTypes}} \cr
##' Request a list of Market for a specific eventType \tab \code{\link{getAllMarkets}} \cr
##' Request details of a Market (excluding prices) \tab \code{\link{getMarket}} \cr
##' Request prices for a Market\tab \code{\link{getMarketPricesCompressed}}\cr
##' Place a bet \tab \code{\link{placeBets}}\cr
##' Cancel a bet before it is matched\tab \code{\link{cancelBets}}\cr
##' Retrieve a list of my Matched/Unmatched bets\tab \code{\link{getMUBets}}\cr
##' Edit an Unmatched bet \tab \code{\link{updateBets}}\cr
##' Retrieve the P&L for a market \tab \code{\link{getMarketProfitAndLoss}}\cr
##' Place a Betfair SP bet \tab \code{\link{placeBets}}\cr
##' Check if a market is in-play now \tab \code{\link{getMarketPricesCompressed}}\cr
##' Check if a market is due to be turned in-play\tab \code{\link{getAllMarkets}}\cr
##' Retrieve a list of Settled bets \tab \code{\link{getBetHistory}}\cr
##' Retrieve your P&L for a market \tab \code{\link{getMarketProfitAndLoss}}
##' }
##'
##'
##'
##' For a description of payed and free access types see
##' \url{http://bdp.betfair.com/index.php?option=com_content&task=view&id=36&Itemid=64}.
##' }
##'
##' \section{Output of \code{betfairly} functions}{
##'
##' All \code{betfairly} API functions can return four types of
##' output, given by the \code{option} parameter which can be:
##'
##' \describe{
##'
##' \item{\code{simple} (the default)}{Simplified output represented by a
##' \code{\link{bfSimpleOutput}} object containing slots \code{bfType}
##' (original betfair class), \code{errorCode} ("OK" if succeed),  and
##' \code{minorErrorCode}  (usually an empty string). See
##' \code{\link{bfSimpleOutput-class}}  for more information.}
##'
##' \item{\code{xml}}{raw XML representation}
##'
##' \item{\code{list}}{recursive list mirroring the structure of the node}
##'
##' \item{\code{S4}}{S4 object as described by the service SOAP protocol. Note
##' what you will need \code{XMLSchema} package for the S4 conversion to work,
##' as it defines some  classes which are not provided with \code{betfairly}
##' package. See \code{\link{bfInitClasses}} for further instructions. }
##'
##' }}
##'
##' \section{Betfair exchange servers}{ Functions to betfair exchange services
##' accept a \code{server} parameter, which can be either "GB" (the default)
##' or "AU". You can set a different default with  \code{options(bfServer =
##' "AU")}
##' }
##'
##' \section{Curl Options}{ Each \code{betfairly} function accepts
##' \code{curlOpts} parameter which is passed directly to
##' \code{\link{curlPerform}},  see the documentation of that function for
##' details.
##' }
##'
##' \section{Reporting Bugs}{Before reporting bugs please see the relevant
##' section in the official Betfair
##' \href{https://docs.developer.betfair.com/betfair/}{documentation} and ensure
##' it's not a betfair service issue. Known isssues are documented for each
##' Betfair API action.}
##'
##' \section{Disclaimer}{ The betfairly package is provided with
##' absolutely no warranty. The documentation of the functional API is an
##' adapted and abbreviated version of official Betfair
##' \href{https://docs.developer.betfair.com/betfair/}{documentation}. Please
##' refer to it for the complete reference.
##' @name betfairly-package
##' @docType package
##' @title Access Betfair API from R
##' @author Vitalie Spinu \email{spinuvit@@gmail.com}
##' @keywords package betfair api
##' @seealso \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/}, \url{https://docs.developer.betfair.com/betfair/}\cr
##' \href{https://bdp.betfair.com/index.php?option=com_weblinks&task=view&catid=59&id=29}{Betfair API Quick Start}
NULL



#### SESSION MANAGEMENT:

##' \code{login} enables customers to log in to the API service and initiates a
##' secure session for the user. Users can have multiple sessions 'alive' at any
##' point in time.\cr\cr
##' \code{logout}  allows you to explicitly end your session.\cr\cr
##' \code{keepAlive} can be used to stop a session timing out. Every
##' call to the Betfair API returns a token, in the sessionToken field, that
##' identifies a login session. Every time your application calls the Betfair
##' API and is returned a sessionToken, the session timeout is reset to
##' approximately 20 minutes. After the timeout has passed, the session is
##' expired and you need to login again.\cr
##' If you want to keep your login session active, but your application has not
##' made any Betfair API calls that would generate a new sessionToken and reset
##' the session timeout, you can call keepAlive to obtain a new sessionToken and
##' reset the session timeout.\cr\cr
##' \code{bfSessionTocken} returns the current session token, if any,  NULL otherwise.
##'
##' \code{bfSessionHandler} (not implemented yet) creates a session handler used
##' to access multiple sessions. A betfair functions can be accessed through
##' \code{obj$foo(...)}, where \code{obj} is the session handler object returned
##' by \code{bfSessionHandler}. All the functions in the handler share common
##' parent environment where the \code{.sessionToken} is stored.
##'
##' Every betfair API request/response must contains a session token.  All
##' functions in betfairly-package store the session token in .GlobalEnv in
##' .sessionToken variable by \code{<<-} assignment. To manage a single account
##' this is appropriate. To manipulate several different sessions at the same
##' time create a handler for each session with \code{bfSessionHandler}.
##'
##' @rdname BF_Session_Management
##' @title Session Management.
##' @aliases >BF_Session_Management bfLogin bfLogout keepAlive bfSessionToken bfSessionHandler
##' @usage bfLogin(username, password, productId=82, ipAddress="0", locationId=0, vendorSoftwareId=0, curlOpts=list())
##' bfLogout(curlOpts = list())
##' keepAlive(curlOpts = list())
##' bfSessionToken()
##' bfSessionHandler()
##' @param username The username with which to login to the API for a new session.
##' @param password The password with which to login to the API for a new session.
##' @param productId The API product ID with which to login to the API for a new
##' session. If you want to use the Free Access API, use 82. If you are a paying
##' API subscriber, use the Id provided when you signed up.
##' @param ipAddress For applications that proxy the user's connection, the IP
##' address of the user's computer. Betfair may inform you in the future if you
##' need to provide this field, otherwise set this field to 0 (the default).
##' @param locationId The location ID with which to login for a new session.
##' @param vendorSoftwareId The vendor software ID with which to login to the
##' API for a new session. This is only relevant for software vendors and is
##' provided when software vendors sign up.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return invisibly a sessionToken string
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
##' @export bfLogin bfLogout keepAlive bfSessionHandler bfSessionToken
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
bfLogin <- function(username, password, productId = 82L, ipAddress = "0", locationId = 0L, vendorSoftwareId = 0L,
                    curlOpts = list()){
    mess <- paste("<soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/'
xmlns:bfg='http://www.betfair.com/publicapi/v3/BFGlobalService/' >
    f<soapenv:Header/>
    <soapenv:Body>
    <bfg:login>
    <bfg:request>
    <ipAddress >", ipAddress, " </ipAddress>
    <locationId>", as.integer(locationId), "</locationId>
    <password>", password, "</password>
    <productId >", as.integer(productId), "</productId>
    <username>", username, "</username>
    <vendorSoftwareId>", vendorSoftwareId, "</vendorSoftwareId>
    </bfg:request>
    </bfg:login>
    </soapenv:Body>
    </soapenv:Envelope>", sep = "")
    bfRequest(mess, "login", url= "https://api.betfair.com/global/v3/BFGlobalService", curlOpts=curlOpts)
    invisible(.sessionToken)
}
bfLogout <- function(curlOpts = list()){
    .bfRequestInternal(operation = "logout", curlOpts = curlOpts)
    invisible(.sessionToken)
}
keepAlive <- function(curlOpts = list()){
    .bfRequestInternal(operation = "keepAlive", curlOpts = curlOpts)
    invisible(.sessionToken)
}
bfSessionToken <- function(){
    if(exists(".sessionToken")) .sessionToken
    else NULL
}
bfSessionHandler <- function(){
    stop("Not implemented yet")
}

#### GET EVENTS:

##' Functions to retrieve betfair events (Games,  sports, politics etc)
##'
##' Allows the customer to retrieve lists of all categories of sports (Games, Event
##' Types) that have at least one market associated with them, regardless of
##' whether that market is now closed for betting. This means that, for example,
##' the service would always return the event types Soccer and Horse Racing and
##' would also return Olympics 2004 or EURO 2004 for a certain period after the
##' markets for those events had closed; it would also return Olympics 2004 or EURO
##' 2004 for a certain period before the markets for those events had opened. The
##' service returns information on future events to allow API programmers to see
##' the range of events that will be available to bet on in the near future.
##'
##' @rdname BF_Events
##' @aliases >BF_Events getAllEventTypes
##' @title Betfair Events
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return A data frame with columns id nextMarketId  and exchangeId; an  xml node or S4 object,  as specified by the \code{output} parameter
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}} \code{\link{getActiveEventTypes}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
##' @export
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
getAllEventTypes <- function(locale,
                             output = getOption("bfOutput"), curlOpts = list()){
    res <- bfGenericRequest(match.call())
    returnBFOutput(res, output, classPostfix = "DF", letMeParseFunc = function(x,...){
        x <- bfArrayToDataFrame2(x[["eventTypeItems"]])
        x <- x[!is.na(x$name), ]
        x <- x[order(x$id), ]
        rownames(x) <- x$name
        x$name <- NULL
        list(x) })
}

##'
##'
##' Allows the customer to retrieve lists of all categories of sporting events
##' (Games, Event Types) that are available to bet on: in other words, all those
##' that have at least one currently active or suspended market associated with
##' them. This means, therefore, that the service would, for example, always return
##' the event types Soccer and Horse Racing but would not return Olympics 2004 or
##' EURO 2004 after those events had finished.
##'
##' @rdname BF_Events
##' @param locale Specify the language for the reply if you want a different
##' language than the account default.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return A data frame with columns id, nextMarketId and exchangeId; an xml node or an S4 object,  as specified by the \code{output} parameter
##' @note The GetActiveEventTypes service is a global service, and it returns information about the events
##' available on both the UK and the Australian exchange servers.
##' @export
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
getActiveEventTypes <- function(locale,
                             output = getOption("bfOutput"), curlOpts = list()) NULL
getActiveEventTypes <- getAllEventTypes
## str(getAllEventTypes())
## str(getActiveEventTypes(output = "S4"))


##'
##'
##' Allows you to navigate through the events hierarchy until you reach details of
##' the betting market for an event that you are interested in.
##'
##' From API 5.0 onwards, the GetEvents service returns details of line and
##' range markets, where these markets are available for an event.  Requests
##' for the GetEvents service take as input a parameter called
##' eventParentID. The value of this parameter is either: the (integer) Id
##' value from one item in an array of eventTypeItems that has been returned by
##' the GetAllEventTypes or GetActiveEventTypes services; or an (integer)
##' eventId value from one item in an array of eventItems that has been
##' returned by an earlier GetEvents request.  Use the GetEvents service
##' repeatedly, specifying a different value for eventParentId in each request,
##' until there are no further events to request (this means you have reached
##' the leafnode of the branch of the events tree you have been navigating).
##' To retrieve full details of a betting market whose details have been
##' returned by the GetEvents service, you need to send a GetMarket request to
##' the exchange server indicated by the market's exchangeId parameter (see
##' CROSS REFERENCE TEXT NEEDS RESOLVING). This GetMarket request must also
##' specify the marketId for the market you are requesting. Both the exchangeId
##' and the marketId are returned by GetEvents. For information about
##' GetMarket, see Chapter 24 .
##'
##' @rdname BF_Events
##' @param eventParentId integer This is either an Id value for a single item (in
##' an array of eventTypeItems returned by GetAllEventTypes or
##' GetActiveEventTypes), or it is an eventId for a single eventItem (in an array
##' of eventItems returned by an earlier GetEvents request).
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetEventsRespSimple} which inherits from
##' \code{\link[=bfSimpleOutput-class]{bfSimpleOutput}} class. With slots
##' \code{eventItems} and \code{marketItems} which are data.frames.
##' @export
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##'
getEvents <- function(eventParentId = 1L, locale,
                      output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call["eventParentId"] <- as.integer(eventParentId)
    res <- bfGenericRequest(call)
    returnBFOutput(res, output,
                   data_slots = c("eventItems", "marketItems"),
                   info_slots = c("eventParentId", "couponLinks"),
                   converters= list(`n2:ArrayOfBFEvent`= bfArrayToDataFrame2,
                     `n2:ArrayOfMarketSummary` = bfArrayToDataFrame2))
}
## getEvents()


#### GET MARKETS
##' Functions to retrieve information about Betfair markets.
##'
##' Retrieve information about all of the markets that are currently
##' active or suspended on the given exchange. You can use this service to quickly
##' analyse the available markets on the exchange, or use the response to build a
##' local copy of the Betfair.com navigation menu. You can limit the response to a
##' particular time period, country where the event is taking place, and event
##' type. Otherwise, the service returns all active and suspended markets.
##'
##' @rdname BF_Markets
##' @aliases >BF_Markets getAllMarkets
##' @title Betfair markets
##' @param eventTypeIds A vector with the events ids to return. If not
##' specified, markets from all event types are returned.
##' @param countries The countries where the event is taking place as an array
##' of ISO3 country codes. If not specified, markets from all countries (or
##' international markets) for the specified exchange are returned.
##' @param fromDate Any R date-time object or  string recognized by
##' as.POSIXlt. Use \code{asBFDateTime} to see how your time input is
##' interpreted. If this is set, the response contains only markets where the
##' market time is not before the specified date.
##' @param toDate Any R date-time object or  string recognized by
##' as.POSIXlt. If this is set, the response contains only markets where the
##' market time is not after the specified date. No limit if not specified.
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return A data.frame containing one market per row and a character string if \code{output = "S4"}.
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
##' @export
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
getAllMarkets <- function(eventTypeIds, countries, fromDate, toDate, locale,
                          server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    if(!missing(eventTypeIds))
        call[["eventTypeIds"]] <- asBF(eventTypeIds, "ArrayOfInt")
    if(!missing(countries))
        call[["counetries"]] <- asBF(countries, "ArrayOfCountryCode")
    if(!missing(fromDate))
        call[["fromDate"]] <- asBFDateTime(fromDate)
    if(!missing(toDate))
        call[["toDate"]] <- asBFDateTime(toDate)
    res <- bfGenericRequest(call)
    returnBFOutput(res, output, classPostfix = "DF",
                   letMeParseFunc = function(res){
                       res <- fromBFXML(res[["marketData"]], forceList = TRUE)
                       if(res=="") return(list(data.frame()))
                       res <- strsplit(strsplit(res, ":", fixed = TRUE)[[1]][-1], "~",fixed = TRUE) ## todo use "[^\\]:" instead? to avoid escapes \:
                       res <- as.data.frame(do.call(rbind, res), stringsAsFactors=FALSE)
                       types <- c(marketID="integer", marketName="character", marketType="factor", marketStatus="factor",
                                  eventDate="character", menuPath="character", eventHierarchy="character",
                                  betDelay="numeric", exchangeID= "integer", countryCode="character",
                                  lastRefresh= "character", nrRunners="integer", nrWinners="integer",
                                  totalMatched="numeric", BSP="factor", turningInPlay="factor")
                       names(res) <- names(types)
                       for(nm in names(res))
                           res[[nm]] <- as(res[[nm]], types[[nm]])
                       res[["eventDate"]] <- toBFPOSIX(res[["eventDate"]])
                       res[["lastRefresh"]] <- toBFPOSIX(res[["lastRefresh"]])
                       list(res)
                   })
}
## head(getAllMarkets(1, c("GBR")))

##'
##'
##' The API GetMarket service allows the customer to input a Market ID and retrieve
##' all static market data for the market requested. To get a Market ID for the
##' betting market associated with an event you are interested in, use the
##' GetEvents command.
##'
##' @rdname BF_Markets
##' @param marketId Integer specifying the market ID.
##' @param includeCouponLinks If you set this parameter to true, the service
##' response contains a list of any coupons that include the market you have
##' requested. If you set the parameter to FALSE (the default), no coupon data is returned.
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetMarketRespSimple} which inherits from
##' \code{\link[=bfSimpleOutput-class]{bfSimpleOutput}} class. Additional slot
##' \code{runners} contains a data frame of event participants.
##'
##' Object of native betfair class \code{GetMarketResp} if \code{output = "S4"}.
##' @export
getMarket <- function(marketId, includeCouponLinks = FALSE, locale = "en",
                      server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call["marketId"] <- as.integer(marketId)
    res <- bfGenericRequest(call)
    returnBFOutput(res, output,  data_slots = "runners", simpleNode = "market",
                   converters= list(`n2:ArrayOfRunner`= bfArrayToDataFrame2))
}

##getMarket("102902130")
## getMarket(102759800L)

##'
##'
##' The API GetMarketInfo service allows you to input a Market ID and retrieve
##' market data for the market requested. To get a Market ID for the betting
##' market associated with an event you are interested in, use the GetEvents
##' command. This is a lite service to compliment the GetMarket service.
##'
##' @rdname BF_Markets
##' @param marketId Integer specifying the market ID.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetMarketInfoRespSimple} which inherits from
##' \code{\link[=bfSimpleOutput-class]{bfSimpleOutput}} class and has no extra slots.
##'
##' If \code{output = "S4"},  object of native betfair class \code{GetMarketInfoResp}.
##' @export
getMarketInfo <- function(marketId,
                          server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call["marketId"] <- as.integer(marketId)
    res <- bfGenericRequest(match.call())
    returnBFOutput(res, output, simpleNode = "marketLite")
}
## getMarketInfo("102759800")


##'
##'
##' Obtain all the current odds and matched amounts on a single runner in a particular event.
##' @rdname BF_Markets
##' @param marketId Integer specifying the market ID.
##' @param selectionId The desired runner id.
##' @param asianLineId Mandatory if the market specified by Market ID is an
##' Asian Market, otherwise optional
##' @param currencyCode Three letter ISO 4217 code.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetMarketTradedVolumeRespSimple} with a slot
##' \code{priceItems} containing a data frame of  total match volumes for each odd.
##' @export
getMarketTradedVolume <- function(marketId,  selectionId, asianLineId, currencyCode,
                                  server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    ## mandatory fields:
    call[c("marketId", "selectionId")] <- c(marketId, selectionId)
    res <- bfGenericRequest(call)
    returnBFOutput(res, output, data_slots = "priceItems", info_slots = "actualBSP",
                   converters = list(`n2:ArrayOfVolumeInfo` = bfArrayToDataFrame2))
}

##'
##'
##' Obtain the current price (odds) and matched amounts at each price on all of the runners in a particular market.
##' @rdname BF_Markets
##' @param marketId Integer specifying the market ID.
##' @param currencyCode Three letter ISO 4217 code.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetMarketTradedVolumeCompressedRespSimple}
##' with two additional slots \code{runners} and \code{volumes}.
##' @export
getMarketTradedVolumeCompressed <- function(marketId, currencyCode,
                                            server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    ## mandatory fields:
    call["marketId"] <- marketId
    res <- bfGenericRequest(call)
    returnBFOutput(res, output, letMeParseFunc = .simple_getMarketTradedVolumeCompressed)
}


#### GET PRICES
##' Functions to retrieve prices on Betfair markets.
##'
##' Retrieve all back and lay stakes for each price on the exchange for a given
##' Market ID in a compressed format. The information returned is similar to the
##' GetDetailAvailableMarketDepth, except it returns the data for an entire market,
##' rather than just one selection.
##'
##' @rdname BF_Prices
##' @aliases >BF_Prices getCompleteMarketPricesCompressed
##' @title Prices on betfair markets.
##' @param marketId Integer specifying the market ID.
##' @param currencyCode Three letter ISO 4217 code.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return a character of length 1 representing the compressed output (simple
##' output is not implemented yet todo::)
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}} \code{\link{getActiveEventTypes}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
##' @export
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
getCompleteMarketPricesCompressed <- function(marketId, currencyCode = "EUR",
                                              server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    res <- bfGenericRequest(match.call())
    returnBFOutput(res,  output, letMeParseFunc = function(x) fromBFXML(x[["completeMarketPrices"]]))
}
## todo:
## getCompleteMarketPricesCompressed("101513259")

##'
##'
##' Retrieve dynamic market data for a given Market ID.
##'
##' @rdname BF_Prices
##' @param marketId integer ID of the required market
##' @param currencyCode character  Three letter ISO 4217 code. If not supplied,
##' users currency is used
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{"GetMarketPricesRespSimple"} with a slot
##' \code{runnerPrices} containing a data frame of back and lay prices for each
##' runner. This function returns the same information as
##' \code{getMarketPricesCompressed} but in a merged, long format.
##' @export
getMarketPrices <- function(marketId, currencyCode,
                            server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call[["marketId"]] <- as.integer(marketId)
    res <- bfGenericRequest(call)
    returnBFOutput(res, output, data_slots="runnerPrices", simpleNode = "marketPrices",
                   converters= list(
                     `n2:ArrayOfPrice`= bfArrayToDataFrame2,
                     `n2:ArrayOfRunnerPrices`=
                     function(x,..., converters){
                         if(is.character(x)) return(if(x == "") NA else x)
                         runners <- fromBFXML(x, list(), converters = converters)
                         if(length(runners) == 0) return(NULL)
                         out <- list()
                         for(i in seq_along(runners)){
                             toBack <-
                                 if(is1NA(toBack <- runners[[i]]$bestPricesToBack)) NULL
                                 else cbind(bestPrice = "toBack", toBack)
                             toLay <-
                                 if(is1NA(toLay <- runners[[i]]$bestPricesToLay)) NULL
                                 else  cbind(bestPrice = "toLay", toLay)
                             runners[[i]]$bestPricesToLay <- runners[[i]]$bestPricesToBack <- NULL
                             out[[i]] <- do.call(cbind, c(runner=i, list( rbind(toBack, toLay)), runners[[i]]))
                         }
                         out <- do.call(rbind , out)
                         rownames(out) <- NULL
                         out
                     }))
}

## getMarket("101513259")
## getMarketPrices("101513259")

##'
##'
##' Retrieve dynamic market data for a given Market ID in a compressed format. This
##' service returns the same information as the Get Market Prices service but
##' returns it in a ~ (tilde) delimited String.
##'
##' @rdname BF_Prices
##' @param marketId Integer specifying the market ID.
##' @param currencyCode Three letter ISO 4217 code.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetMarketPricesCompressedRespSimple} containing slots \code{runners} and \code{prices}.
##' @export
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
getMarketPricesCompressed <- function(marketId, currencyCode,
                                      server = getOption("bfServer"), output= getOption("bfOutput"), curlOpts = list()){
    res <- bfGenericRequest(match.call())
    ## fromBFXML(res[["marketPrices"]])
    returnBFOutput(res, output, letMeParseFunc = .simple_getMarketPricesCompressed)
}
## getMarketPricesCompressed(mid)
## getMarketPrices(102892658)


#### BET HISTORY:
##' With \code{getBetHistory}, \code{getMUBets} and \code{getMUBetsLite} you
##' access information about all your bets.  With getBet, getBetLite and
##' getBetMatchesLite you can access detailed information about your specific
##' bets.
##'
##' Retrieve information about a particular bet. Each request will retrieve all
##' components of the desired bet.
##'
##' You can retrieve Cancelled, Lapsed, and Voided bets from only settled
##' markets and these bets are available for a maximum of 10 days from the date
##' the market was settled.
##' @title Access your bets
##' @rdname BF_Bet_History
##' @param betId The unique bet identifier
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return A list of class \code{GetBetRespSimple} containing slot
##' \code{matches} with info  about matched portions of the bet.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
getBet <- function(betId,
                   server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(betId = betId, operation = "getBet", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, data_slots = "matches", simpleNode = "bet",
                   converters = list('n2:ArrayOfMatch' = bfArrayToDataFrame2))
}

##'
##'
##' This is the lite version of the GetBet service.
##' @rdname BF_Bet_History
##' @param betId The unique bet identifier
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return A list of class \code{GetBetLiteRespSimple} with no additional
##' slots.  Contains a subset of information from data part of \code{getBet}
##' response.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
getBetLite <- function(betId,
                       server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(betId = betId, operation = "getBetLite", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, simpleNode = "betLite")
}


##'
##' This is a lite version of the GetBet service that returns information on matched bets.{
##' @rdname BF_Bet_History
##' @param betId The unique bet identifier
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Data frame of class \code{GetBetMatchesLiteRespSimple} containing
##' subset of information from \code{@@matches} slot in \code{getBet} response.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
getBetMatchesLite <- function(betId,
                              server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(betId = betId, operation = "getBetMatchesLite", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["matchLites"]])))
}

##'
##'
##' Retrieve information about all your matched and unmatched bets on a
##' particular exchange server.  You should be aware that voided bets are not
##' returned by getMUBets. Your application should track the number of matched
##' and unmatched bets against the number of bets returned by getMUBets in order
##' to detect a voided bet.
##' @rdname BF_Bet_History
##' @param marketId For \strong{getMUBets} and \strong{getMUBetsLite}: If
##' marketId is present and non-zero, then bets placed on the specified market
##' are returned and any bet ids specified in betIds are ignored.\cr For
##' \strong{getBetHistory} Returns the records of your matched or unmatched bets
##' for the specified market. If you use this parameter you must not specify
##' \code{eventTypeId} array. Note that, if you specify a marketId, you must also
##' specify either M or U as the value for the \code{betTypesIncluded} parameter.
##' @param betIds Specifies the betId of each bet you want returned. The maximum
##' number of bets you can include in the array is 200. If you include marketId
##' in the request and marketId contains a non-zero value, then betIds is
##' ignored. If you specify a betId, then you must specify MU for betStatus.
##' @param betStatus M, U or MU. The status of the bets to return (matched, unmatched, or
##' both) - please see betfairly Simple Data Types . If you specify a betId, then
##' you must specify MU.
##' @param matchedSince Specifies a date and time to start from for the list of
##' returned bets. Any R date-time object or  string recognized by
##' as.POSIXlt. Use \code{asBFDateTime} to see how your time input is
##' interpreted. If you use the matchedSince parameter and you have specified a
##' betStatus of MU, the bets returned will ignore any limit you set (using
##' recordCount) for the number of records to be returned. Specifying a
##' betStatus of MU causes the API to return your unmatched bets along with the
##' matched ones.
##' @param orderBy The order of returned results. Valid orders are BET_ID,
##' PLACED_DATE, and MATCHED_DATE.
##' @param sortOrder ASC or DESC. Whether the results are in ascending or descending order
##' @param recordCount Maximum number of records to return. The maximum number allowed is 200.
##' @param startRecord The first record number to return (supports
##' paging). Record numbering starts from 0. For example, to retrieve the third
##' record and higher, set startRecord to 2.
##' @param excludeLastSecond If true, the API excludes bets placed or matched
##' that occurred less than one second before the GetMUBets call. Set this to
##' true if you want to ensure that the response does not include bets that may
##' have changed state between the time you sent the request and before the
##' response was generated. If false, all bets are returned. Therefore, you may
##' receive a response that indicates an unmatched bet that has actually been
##' matched during the time taken for the API to respond.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{xxx} containing slot
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
getMUBets <- function(marketId, betIds, betStatus = "MU", matchedSince, orderBy = "BET_ID",
                      sortOrder = "ASC", recordCount = 200, startRecord = 0, excludeLastSecond = FALSE,
                      server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call[c("betStatus", "orderBy", "recordCount", "sortOrder", "startRecord")] <-
        c(betStatus, orderBy, recordCount, sortOrder, startRecord)
    if(!missing(betIds))
        call[["betIds"]] <- asBF(betIds, "ArrayOfBetId")
    if(!missing(matchedSince))
        call[["matchedSince"]] <- asBFDateTime(matchedSince)
    out <- bfGenericRequest(call)
    returnBFOutput(out, output, classPostfix = "DF", letMeParseFunc = function(out){
        df <- bfArrayToDataFrame2(out[["bets"]])
        if(length(df)){
            dNames <- grep("Date", names(df), fixed = TRUE)
            df <- df[, c((1:length(df))[-dNames], dNames)]
        }
        list(df)
    })
}

##'
##'
##' This is a lite version of the getMUBets service.
##' @rdname BF_Bet_History
##' @param marketId Integer specifying the market ID.
##' @param betIds A vector specifying the betId of each bet you want
##' returned. The maximum number of bets you can include in the array is 200. If
##' you include marketId in the request and marketId contains a non-zero value,
##' then betIds is ignored. If you specify a betId, then you must specify MU for
##' betStatus.
##' @param betStatus
##' @param matchedSince
##' @param orderBy
##' @param sortOrder
##' @param recordCount
##' @param startRecord
##' @param excludeLastSecond
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @export
getMUBetsLite <- function(marketId, betIds, betStatus = "MU", matchedSince, orderBy = "BET_ID",
                          sortOrder = "ASC", recordCount = 200, startRecord = 0, excludeLastSecond = FALSE,
                          server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call[c("betStatus", "orderBy", "recordCount", "sortOrder", "startRecord")] <-
        c(betStatus, orderBy, recordCount, sortOrder, startRecord)
    if(!missing(betIds))
        call[["betIds"]] <- asBF(betIds, "ArrayOfBetId")
    if(!missing(matchedSince))
        call[["matchedSince"]] <- asBFDateTime(matchedSince)
    out <- bfGenericRequest(call)
    returnBFOutput(out, output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["betLites"]])))
}


##'
##'
##' @rdname BF_Bet_History
##' @param marketId Integer specifying the market ID.
##' @param eventTypeIds An array of event types to return. For matched and
##' unmatched bets only, you can leave it unspecified  and specify zero (the default) as the
##' marketId to receive records of all your bets on the exchange.
##' @param detailed [logical] Whether to show details of all the matches on a single bet
##' @param sortBetsBy [ASC, DESC] How the bets are ordered.
##' @param betTypesIncluded [C Cancelled, L Lapsed, M Matched, MU Matched and
##' Unmatched, S Settled (default), U Unmatched, V Voided] Indicates the status of the
##' bets to include in the response. If your \code{betHistory} request is for a
##' specific market (in other words, if you have specified a marketId in your
##' request), then you must specify either M or U as the value for
##' betTypesIncluded. Otherwise you will receive an INVALID_BET_STATUS
##' error. Only settled markets return cancelled, void, or lapsed bets.
##' @param marketTypesIncluded [A Asian Handicap, L Line, O Odds (default), R
##' Range] Indicates the types of market that you want your betting history
##' returned for.
##' @param placedDateFrom Any R date/date-time object is accepted or any character
##' string recognized by as.POSIXlt. Default to current day at 00:00.
##' @param placedDateTo Any R date/date-time object. Default to Sys.time().
##' @param recordCount The maximum number of records to return. This number must
##' be between 1 and 100, inclusive.
##' @param startRecord The first record number to return (supports
##' paging). Record numbering starts from 0. For example, to retrieve the third
##' record and higher, set startRecord to 2.
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param timezone Specify an alternative time-zone from the user account default.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetBetHistoryRespSimple} containing slots
##' \code{betHistoryItems} - a data frame with one  bet per row  and
##' \code{matches} - a data frame with all the matches if the \code{details}
##' parameter  was set to TRUE.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
getBetHistory <- function(marketId = 0, eventTypeIds = NULL, detailed = FALSE, sortBetsBy = "NONE",
                          betTypesIncluded = "S", marketTypesIncluded = "O",
                          placedDateFrom = Sys.Date(), placedDateTo = Sys.time(),
                          recordCount = 100, startRecord = 0,
                          locale, timezone,
                          server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    request <- list(eventTypeIds = asBF(eventTypeIds, "ArrayOfInt"), detailed = detailed, betTypesIncluded = betTypesIncluded,
                    marketTypesIncluded = asBF(marketTypesIncluded, "ArrayOfMarketTypeEnum"),
                    placedDateFrom = asBFDateTime(placedDateFrom), placedDateTo = asBFDateTime(placedDateTo),
                    recordCount = recordCount, startRecord = startRecord, sortBetsBy = sortBetsBy, curlOpts = curlOpts, server = server)
    if(!missing(marketId)) request[["marketId"]] <- marketId
    if(!missing(locale)) request[["locale"]] <- locale
    if(!missing(timezone)) request[["timezone"]] <- timezone
    out <- do.call(.bfRequestInternal, c(request, operation = "getBetHistory"))
    returnBFOutput(out, output, letMeParseFunc = .simple_GetBetHistory)
}

##'
##'
##' Retrieve Profit and Loss information for the user account in a given market.
##' The limitations for the service in the initial release are:
##'
##'    * Profit and loss for single and multi-winner odds markets is implemented
##' however it won't calculate worstCaseIfWin nor futureIfWin.
##'
##'    * The calculation for AH markets will include worstCaseIfWin but not futureIfWin.
##' @rdname BF_Bet_History
##' @param marketID The market ID for which the profit and loss for the user is
##' to be returned
##' @param includeSettledBets logical If TRUE then the P&L calculation for each runner
##' includes any profit and loss from any bets on runners that have already been
##' settled. The default is FALSE, which matches the default on Betfair.com.
##' @param includeBspBets If TRUE, BSP bets are returned as part of the P&L
##' @param netOfCommission If TRUE return P&L net of users current commission rate
##' for this market including any special tariffs, default is FALSE.
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetMarketProfitAndLossRespSimple} containing slot \code{annotations} which is a data frame with P&L data.
##' @export
getMarketProfitAndLoss <- function(marketID, includeSettledBets = FALSE,  includeBspBets = TRUE,   netOfCommission = FALSE, locale,
                                   server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    call <- match.call()
    call[c("marketID", "includeBspBets")] <- c(marketID, includeBspBets)
    out <- bfGenericRequest(call)
    returnBFOutput(out, output, exclude_info_slots = c("header", "errorCode", "minorErrorCode"),
                   data_slots = "annotations", converters = list('n2:ArrayOfProfitAndLoss' = bfArrayToDataFrame2))
}


### BET PLACEMENT

##' Place, update and cancel multiple bets at a time. \cr \cr For
##' \code{placeBets} and \code{updateBets}, there are two equivalent ways of
##' supplying the bet info. First, by passing a list of corresponding objects
##' (\code{bfBet} and \code{bfBetUpdate}) as \code{bets} argument. Second, a
##' vectorized (\code{mapplyish}) way,  by supplying vectors to corresponding
##' arguments. Vectorized arguments are recycled to the same length if
##' needed. These arguments are all in plural and are not documented below.
##'
##'
##' Constructor of \code{bfBet} object. You supply a list of these objects as
##' \code{bets} argument to \code{placeBets}.
##'
##' ##' The required fields in bets are dependent on the category of bet. The
##' following table shows the required fields for each bet category.
##'
##' Table 1. Valid Bet Category request field combinations
##' \tabular{lrrrr}{
##' Bet Category \tab Price \tab Size \tab BspLiability \tab BetPersistenceType \cr
##' Exchange \tab Yes \tab Yes \tab No \tab Yes \cr
##' Market on Close \tab No \tab No \tab Yes \tab No \cr
##' Limit on Close \tab Yes \tab No \tab Yes \tab No
##' }
##' @title Functions to place, update and cancel bets.
##' @rdname BF_Bet_Placement
##' @aliases >BF_Bet_Placement bfBet
##' @param marketId Integer specifying the market ID.
##' @param selectionId  ID of the desired runner or selection within the market
##' @param price numeric  The price (odds) you want to set for the bet. Valid
##' values are 1.01 to 1000. For a BSP Limit on Close bet, specify the desired
##' price limit. For a Back bet, the minimum price you want. If the Starting
##' Price is lower than this amount, then your bet is not matched. For a Lay
##' bet, the maximum acceptable price. If the Starting Price is higher than this
##' amount, then your bet is not matched. If the specified limit is equal to the
##' starting price, then it may be matched, partially matched, or may not be
##' matched at all, depending on how much is needed to balance all bets against
##' each other (MOC, LOC and normal exchange bets).
##' @param size numeric The stake (amount) for an exchange bet. The minimum
##' amount for a back bet is 2 (or equivalent). If the betPersistenceType is
##' set to SP, then the minimum amount for a lay bet is 10 (or equivalent),
##' otherwise, the minumum lay bet amount is 2 (or equivalent).
##' @param betType 'B' - back, 'L'- lay. See details.
##' @param bspLiability numeric  This is the maximum amount of money you want to risk for
##' a BSP bet. The minimum amount for a back bet is 2 (or equivalent). The
##' minimum amount for a lay bet is 10 (or equivalent) For a back bet, this is
##' equivalent to the stake on a normal exchange bet. For a lay bet, this is the
##' equivalent to the liability on a normal exchange bet. If after the market is
##' reconciled, the actual stake is calculated once the price is known.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param betCategoryType E, M or L. 'E' - Exchange bet, 'M' - Market on
##' Close SP bet, 'L' - Limit on Close SP bet. If you specify Limit on Close,
##' specify the desired limit using the price argument. See details.
##' @param betPersistenceType NONE, IP or SP. Specify what happens to an
##' unmatched (or partially unmatched) exchange bet when the market turns
##' in-play. If betCategoryType is an SP bet, betPersistenceType must be set to
##' NONE. See details.
##' @return String of class \code{bfBet}.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
bfBet <- function(marketId, selectionId, price, size, betType = "B", bspLiability = 0.0,
                  betCategoryType = "E", betPersistenceType = "NONE",
                  server = getOption("bfServer")){
    call <- match.call()
    switch(betCategoryType,
           E = call[c("betCategoryType", "price", "size", "betPersistenceType", "bspLiability")] <- c(betCategoryType, price, size, betPersistenceType, bspLiability),
           M = call[c("betCategoryType", "bspLiability", "betPersistenceType")] <- c(betPersistenceType, bspLiability, betPersistenceType),
           L = call[c("betCategoryType", "price", "bspLiability")] <- c(betCategoryType, price, bspLiability),
           stop("betCategoryType must be one of E, M, L. Supplied : ", betCategoryType))
    if(!(betPersistenceType %in% c("NONE", "IP", "SP")))
        stop("betPersistenceType must be NONE, IP, or SP. Supplied : ", betPersistenceType)
    if(!(betType %in% c("B", "L")))
        stop("betType must be B, or L. Supplied : ", betType)
    call[c("marketId", "selectionId", "betType")] <- c(marketId, selectionId, betType)
    call[[1]] <- as.name("list")
    structure(bfCollapseParams(eval(call, envir = parent.frame(1L))), class = "bfBet")
}

##'
##'
##' Place multiple (1 to 60) bets on a single Market. There is an instance of
##' PlaceBetsResp returned in the output for each instance of PlaceBets in the
##' input. The success or failure of the individual bet placement operation is
##' indicated by the Success Boolean.
##'
##' \describe{
##' \item{Bet Types}{
##'
##' You can specify, for each bet, if you want to place a Back bet or a Lay bet.
##'
##' * B - Back bets win when the selection is settled as the winner in the market.
##' * L - Lay bets win when the selection is settled as a looser in the market.
##'
##' For more information on Bet types, see the Betfair website help.
##' }
##' \item{Bet Categories}{
##'
##' You can specify, for each bet, whether the bet is a regular exchange bet, or
##' a Betfair Market on Close (or Starting Price) bet (with or without a price
##' limit).
##'
##' * E - Exchange bets are placed on the market and are matched against bets at the
##' specified or better price. Exchange bets are matched on a first in, first
##' matched basis.
##'
##' * M - Market on Close (MOC) bets remain unmatched until the market is
##' reconciled. They are matched and settled at a price that is representative
##' of the market at the point the market is turned in-play. The market is
##' reconciled to find a starting price and MOC bets are settled at whatever
##' starting price is returned. MOC bets are always matched and settled, unless
##' a starting price is not available for the selection. Market on Close bets
##' can only be placed before the starting price is determined.
##'
##' * L - Limit on Close (LOC) bets are matched if, and only if, the returned
##' starting price is better than a specified price. In the case of back bets,
##' LOC bets are matched if the calculated starting price is greater than the
##' specified price. In the case of lay bets, LOC bets are matched if the
##' starting price is less than the specified price. If the specified limit is
##' equal to the starting price, then it may be matched, partially matched, or
##' may not be matched at all, depending on how much is needed to balance all
##' bets against each other (MOC, LOC and normal exchange bets)
##' }
##' \item{Bet Persistence}{
##'
##' You can specify what happens to an Exchange bet that is unmatched when the
##' market is reconciled and the starting price is calculated.
##'
##' * NONE - The unmatched bet is cancelled when the market is reconciled and turned in-play.
##'
##' * IP - The unmatched bet stays as an unmatched bet when the market is turn in-play.
##'
##' * SP - The unmatched bet becomes a Market on Close bet and is matched at the starting price.
##'
##' }}
##'
##' @rdname BF_Bet_Placement
##' @param bets For \emph{\code{placeBets}}  an \code{bfBet} object or a list (of max 60) such objects.\cr
##' For \emph{\code{cancelBets}}  a vector of bet ids to be canceled (max 40).\cr
##' For \emph{\code{updateBets}}  an \code{bfBetUpdate} object or a list (of max 15) such objects.
##' @param marketIds Vector of integers specifying the market IDs.
##' @param selectionIds _
##' @param prices _
##' @param sizes _
##' @param betTypes _
##' @param bspLiabilities _
##' @param betCategoryTypes _
##' @param betPersistenceTypes _
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Data frame with info on the success of placed bets, one bet per row.
##' @export
placeBets <- function(bets = list(),
                      marketIds, selectionIds, prices, sizes, betTypes = "B", bspLiabilities = 0.0,
                      betCategoryTypes = "E", betPersistenceTypes = "NONE",
                      server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    if(is(bets, "bfBet"))
        bets <- list(bets)
    if(length(bets) && !all(sapply(bets, is, "bfBet")))
        stop("All elements of 'bets' argument must be of class 'bfBet' as produced by 'bfBet' function.")
    if(!missing(marketIds))
        bets <- c(bets, mapply(bfBet, marketIds, selectionIds, prices, sizes, betTypes, bspLiabilities,
                               betCategoryTypes, betPersistenceTypes))
    out <- .bfRequestInternal(bets = asBF(bets, "ArrayOfPlaceBets"), operation = "placeBets", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["betResults"]])))
}

##'
##'
##' Cancel multiple unmatched (1 to 40) bets placed on a single Market. The
##' success or failure of the individual bet cancellation operation will be
##' indicated by the Success Boolean. If a portion of the original bet is
##' already matched, cancelBets cancels the unmatched portion of the bet.
##' @rdname BF_Bet_Placement
##' @param bets
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Data frame with info on canceled bets, one bet per row.
##' @export
cancelBets <- function(bets,
                       server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    stopifnot(is.vector(bets))
    out <- .bfRequestInternal(bets = asBF(bets, "ArrayOfCancelBets"), operation = "cancelBets", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["betResults"]])))
}

##'
##'
##' [payed] Cancel all unmatched bets (or unmatched portions of bets) placed on
##' one or more Markets. You might use this service to quickly close out a
##' position on a market.
##' @rdname BF_Bet_Placement
##' @param markets Vector of market IDs.
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{xxx} containing slot
##' @export
cancelBetsByMarket <- function(markets,
                               server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    stopifnot(is.vector(markets))
    out <- .bfRequestInternal(markets = asBF(markets, "ArrayOfInt"),  operation = "cancelBetsByMarket", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, classPostfix = "DF",
                   letMeParseFunc = function(out) bfArrayToDataFrame2(out[["results"]]))
}

##'
##'
##' Constructor of \code{bfBetUpdate} object. You supply a list of these  objects
##' as \code{bets} argument to \code{cancelBets}.
##' @rdname BF_Bet_Placement
##' @param betId The unique identifier for the bet.
##' @param newPrice New odds desired on the bet For BSP Limit on Close bets,
##' newPrice should be set to the new limit desired. For BSP Limit on Close back
##' bets, you can only change the limit downward. For BSP Limit on Close lay
##' bets, you can only change the limit upward.
##' @param oldPrice For an exchange bet, original odds on the bet.
##' @param newSize New stake desired on the bet
##' @param oldSize For an exchange bet, original stake on the bet
##' @param newBetPersistenceType New persistence type on an exchange bet. Only
##' valid before the market turns in-play.
##' @param oldBetPersistenceType Original persistence type on an exchange
##' bet. Only valid before the market turns in-play.
##' @return String of class \code{bfBetUpdate}.
##' @export
bfBetUpdate <- function(betId, newPrice, oldPrice, newSize, oldSize,newBetPersistenceType,  oldBetPersistenceType){
    call <- match.call()
    call[c("betId")] <- c(betId)
    call[[1]] <- as.name("list")
    structure(bfCollapseParams(eval(call, envir = parent.frame(1))), class = "bfBetUpdate")
}

##'
##'
##' Edit multiple (1 to 15) bets on a single Market. The success or failure of
##' the individual bet editing operation is indicated by the Success Boolean.
##'
##' If newPrice and newSize are both specified the newSize value is ignored. For
##' example, an original bet is placed for 100 with odds of 1.5: UpdateBets is
##' called with newSize = 200, newPrice = 2. The original bet is cancelled and a
##' new bet is place for 100 with odds of 2.
##'
##' @rdname BF_Bet_Placement
##' @param bets Can be an \code{bfBetUpdate} object or a list (of max 15) such objects.
##' @param betIds _
##' @param newPrices _
##' @param oldPrices _
##' @param newSizes _
##' @param oldSizes _
##' @param newBetPersistenceTypes _
##' @param oldBetPersistenceTypes _
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Data frame with info on the success of bet updates,  one bet per row.
##' @export
updateBets <- function(bets = list(), betIds, newPrices, oldPrices, newSizes, oldSizes,  newBetPersistenceTypes, oldBetPersistenceTypes,
                       server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    if(is(bets, "bfBetUpdate"))
        bets <- list(bets)
    if(length(bets) && !all(sapply(bets, is, "bfBetUpdate")))
        stop("All elements of 'bets' argument must be of class 'bfBet' as produced by 'bfBet' function.")
    if(!missing(betIds)){
        args <- list(betId = betIds)
        if(!missing(newPrices)) args[["newPrice"]] <- newPrices
        if(!missing(oldPrices)) args[["oldPrice"]] <- oldPrices
        if(!missing(newSizes)) args[["newSize"]] <- newSizes
        if(!missing(oldSizes)) args[["oldSize"]] <- oldSizes
        if(!missing(newBetPersistenceTypes)) args[["newBetPersistenceType"]] <- newBetPersistenceTypes
        if(!missing(oldBetPersistenceTypes)) args[["oldBetPersistenceType"]] <- oldBetPersistenceTypes
        bets <- c(bets, do.call("mapply",  c("bfBetUpdate", args)))
    }
    out <- .bfRequestInternal(bets = asBF(bets, "ArrayOfUpdateBets"), operation = "updateBets", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["betResults"]])))
}



#### ACCOUNT MANAGEMENT

##' Various functions to access information about your account and wallets.
##'
##' Retrieve information about your local wallet on a particular exchange
##' server. For an explanation of the concept of wallets, see "Using
##' Region-specific Wallets for Placing Bets" on page 12 in Betfair API
##' Developer Documentation.
##' @title Account management.
##' @rdname BF_Acount_Management
##' @aliases >BF_Acount_Management getAccountFunds
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{GetAccountFundsRespSimple} with no extra slots.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
getAccountFunds <- function(server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(operation = "getAccountFunds", curlOpts = curlOpts, server = server)
    returnBFOutput(out, output = output,
                   exclude_info_slots = c("header", "errorCode", "minorErrorCode"))
}


##'
##'
##' Obtain information about transactions involving your local wallet on an exchange server.
##' @rdname BF_Acount_Management
##' @param startDate Return records on or after this date.
##' @param endDate Return records on or before this date.
##' @param startRecord The first record number to return (supports
##' paging). Record numbering starts from 0. For example, to retrieve the third
##' record and higher, set startRecord to 2.
##' @param recordCount The maximum number of records to return.
##' @param itemsIncluded Determines what type of statements items to return.
##' @param locale Specify the language for the reply if you want a different language than the account default.
##' @param ignoreAutoTransfers _
##' @param server "GB" (default)  or "AU" - a Betfair exchange server to
##' use. You can set the default with \code{options(bfServer = "AU")}.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Data.frame of class \code{GetAccountStatementRespSimpleDF} with no extra slots.
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}}
##' @references \url{http://code.google.com/p/betfairly/},  \url{https://docs.developer.betfair.com/betfair/}
getAccountStatement <- function(startDate = Sys.Date()-1, endDate = Sys.time(),
                                startRecord = 0, recordCount  = 100, itemsIncluded ="ALL", locale, ignoreAutoTransfers = TRUE,
                                server = getOption("bfServer"), output = getOption("bfOutput"), curlOpts = list()){
    req <- list(startDate = asBFDateTime(startDate), endDate = asBFDateTime(endDate),
                startRecord = startRecord, recordCount = recordCount, itemsIncluded = itemsIncluded
                )
    if(!missing(locale)) req[["locale"]] <- locale
    out <- do.call(.bfRequestInternal, c(req, operation = "getAccountStatement", curlOpts = curlOpts, server = server))
    returnBFOutput(out, output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["items"]])))
}

##'
##'
##' Return information on your API subscription.
##' @rdname BF_Acount_Management
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{getSubscriptionInfo} with no extra slots.
##' @export
getSubscriptionInfo <- function( output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(operation = "getSubscriptionInfo", curlOpts = curlOpts)
    returnBFOutput(out, output, classPostfix = "DF",
                   letMeParseFunc = function(out) list(bfArrayToDataFrame2(out[["subscriptions"]])))
}



##'
##'
##' Transfer funds between your UK and Australian account wallets. The
##' concept of account wallets has been introduced in release 5.0 of the Betfair
##' API. Instead of a single account holding all of a customer's funds for
##' betting on sports events, there are now two "wallets" for each customer's
##' account: one for betting on the UK exchange server and one for betting on
##' the Australian exchange server.
##' @rdname BF_Acount_Management
##' @param amount _
##' @param sourceWalletId 	The wallet that you are requesting the funds to
##' be transferred from. There are two possible wallets: 1 = UK Sports Betting
##' wallet 2 = Australian Sports Betting wallet
##' @param targetWalletId 	The wallet that you are requesting the funds to
##' be transferred from.
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{TransferFundsRespSimple} with no extra slots.
##' @export
transferFunds <- function(amount, sourceWalletId = 1,  targetWalletId = 2,
                          output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(amount = amount, sourceWalletId = sourceWalletId, targetWalletId = targetWalletId, operation = "transferFunds")
    returnBFOutput(out, output, info_slots = "monthlyDepositTotal")
}

##'
##'
##' Retrieve information about the user account, such as the registered address,
##' e-mail address, phone numbers, etc.
##' @rdname BF_Acount_Management
##' @param output Indicates the form of the returned value. Can be "simple"
##' (default), "xml", "list" or "S4". See \code{\link{betfairly-package}}.
##' @param curlOpts RCurl options passed directly to
##' \code{\link{curlPerform}}. You can also set the defaults with
##' \code{options(bfCurlOpts = list(opt1 = val1, opt2 = val2, ...))}.
##' @return Object of class \code{ViewProfileRespSimple} with not extra slots.
##' @export
viewProfile <- function(output = getOption("bfOutput"), curlOpts = list()){
    out <- .bfRequestInternal(operation = "viewProfile")
    returnBFOutput(out, output, exclude_info_slots = c("header", "errorCode", "minorErrorCode"))
}

## Local Variables:
## ess-roxy-template-alist: (
##  ("description" . "..description")
##  ("details" . "..details")
##  ("title" . "")
##  ("rdname" . "")
##  ("param" . "")
##  ("return" . "Object of class \\code{xxx} containing slot ")
##  ("author" . "Vitalie Spinu (\\email{spinuvit@@gmail.com})")
##  ("export" . "")
##  ("seealso" . "\\code{\\link{betfairly-package}} \\code{\\link{bfSimpleOutput-class}}")
##  ("references" . "\\url{https://docs.developer.betfair.com/betfair/}")
##  )
## end:
