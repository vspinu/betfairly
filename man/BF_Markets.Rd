\name{getAllMarkets}
\alias{>BF_Markets}
\alias{getAllMarkets}
\alias{getMarket}
\alias{getMarketInfo}
\alias{getMarketTradedVolume}
\alias{getMarketTradedVolumeCompressed}
\title{Betfair markets}
\usage{
  getAllMarkets(eventTypeIds, countries, fromDate, toDate,
    locale, server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMarket(marketId, includeCouponLinks = FALSE,
    locale = "en", server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMarketInfo(marketId, server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMarketTradedVolume(marketId, selectionId, asianLineId,
    currencyCode, server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMarketTradedVolumeCompressed(marketId, currencyCode,
    server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())
}
\arguments{
  \item{eventTypeIds}{A vector with the events ids to
  return. If not specified, markets from all event types
  are returned.}

  \item{countries}{The countries where the event is taking
  place as an array of ISO3 country codes. If not
  specified, markets from all countries (or international
  markets) for the specified exchange are returned.}

  \item{fromDate}{Any R date-time object or string
  recognized by as.POSIXlt. Use \code{asBFDateTime} to see
  how your time input is interpreted. If this is set, the
  response contains only markets where the market time is
  not before the specified date.}

  \item{toDate}{Any R date-time object or string recognized
  by as.POSIXlt. If this is set, the response contains only
  markets where the market time is not after the specified
  date. No limit if not specified.}

  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{server}{"GB" (default) or "AU" - a Betfair exchange
  server to use. You can set the default with
  \code{options(bfServer = "AU")}.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{marketId}{Integer specifying the market ID.}

  \item{includeCouponLinks}{If you set this parameter to
  true, the service response contains a list of any coupons
  that include the market you have requested. If you set
  the parameter to FALSE (the default), no coupon data is
  returned.}

  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{server}{"GB" (default) or "AU" - a Betfair exchange
  server to use. You can set the default with
  \code{options(bfServer = "AU")}.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{marketId}{Integer specifying the market ID.}

  \item{server}{"GB" (default) or "AU" - a Betfair exchange
  server to use. You can set the default with
  \code{options(bfServer = "AU")}.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{marketId}{Integer specifying the market ID.}

  \item{selectionId}{The desired runner id.}

  \item{asianLineId}{Mandatory if the market specified by
  Market ID is an Asian Market, otherwise optional}

  \item{currencyCode}{Three letter ISO 4217 code.}

  \item{server}{"GB" (default) or "AU" - a Betfair exchange
  server to use. You can set the default with
  \code{options(bfServer = "AU")}.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{marketId}{Integer specifying the market ID.}

  \item{currencyCode}{Three letter ISO 4217 code.}

  \item{server}{"GB" (default) or "AU" - a Betfair exchange
  server to use. You can set the default with
  \code{options(bfServer = "AU")}.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}
}
\value{
  A data.frame containing one market per row and a
  character string if \code{output = "S4"}.

  Object of class \code{GetMarketRespSimple} which inherits
  from \code{\link[=bfSimpleOutput-class]{bfSimpleOutput}}
  class. Additional slot \code{runners} contains a data
  frame of event participants.

  Object of native betfair class \code{GetMarketResp} if
  \code{output = "S4"}.

  Object of class \code{GetMarketInfoRespSimple} which
  inherits from
  \code{\link[=bfSimpleOutput-class]{bfSimpleOutput}} class
  and has no extra slots.

  If \code{output = "S4"}, object of native betfair class
  \code{GetMarketInfoResp}.

  Object of class \code{GetMarketTradedVolumeRespSimple}
  with a slot \code{priceItems} containing a data frame of
  total match volumes for each odd.

  Object of class
  \code{GetMarketTradedVolumeCompressedRespSimple} with two
  additional slots \code{runners} and \code{volumes}.
}
\description{
  Functions to retrieve information about Betfair markets.

  The API GetMarket service allows the customer to input a
  Market ID and retrieve all static market data for the
  market requested. To get a Market ID for the betting
  market associated with an event you are interested in,
  use the GetEvents command.

  The API GetMarketInfo service allows you to input a
  Market ID and retrieve market data for the market
  requested. To get a Market ID for the betting market
  associated with an event you are interested in, use the
  GetEvents command. This is a lite service to compliment
  the GetMarket service.

  Obtain all the current odds and matched amounts on a
  single runner in a particular event.

  Obtain the current price (odds) and matched amounts at
  each price on all of the runners in a particular market.
}
\details{
  Retrieve information about all of the markets that are
  currently active or suspended on the given exchange. You
  can use this service to quickly analyse the available
  markets on the exchange, or use the response to build a
  local copy of the Betfair.com navigation menu. You can
  limit the response to a particular time period, country
  where the event is taking place, and event type.
  Otherwise, the service returns all active and suspended
  markets.
}
\references{
  \url{http://code.google.com/p/betfairly/},
  \url{https://docs.developer.betfair.com/betfair/}
}
\seealso{
  \code{\link{betfairly-package}}
  \code{\link{bfSimpleOutput-class}}
}

