\name{getCompleteMarketPricesCompressed}
\alias{>BF_Prices}
\alias{getCompleteMarketPricesCompressed}
\alias{getMarketPrices}
\alias{getMarketPricesCompressed}
\title{Prices on betfair markets.}
\usage{
getCompleteMarketPricesCompressed(marketId, currencyCode = "EUR",
  server = getOption("bfServer"), output = getOption("bfOutput"),
  curlOpts = list())

getMarketPrices(marketId, currencyCode, server = getOption("bfServer"),
  output = getOption("bfOutput"), curlOpts = list())

getMarketPricesCompressed(marketId, currencyCode,
  server = getOption("bfServer"), output = getOption("bfOutput"),
  curlOpts = list())
}
\arguments{
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

  \item{marketId}{integer ID of the required market}

  \item{currencyCode}{character Three letter ISO 4217 code.
  If not supplied, users currency is used}

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
A list of class
\code{GetCompleteMarketPricesCompressedRespSimple} with
three additional slots containing data.frames
\code{removedRunners}, \code{runners} and \code{prices}.
Use \code{\link{merge}} for joining these by the common
field \code{runners}.

Object of class \code{"GetMarketPricesRespSimple"} with a
slot \code{runnerPrices} containing a data frame of back
and lay prices for each runner. This function returns the
same information as \code{getMarketPricesCompressed} but in
a merged, long format.

Object of class \code{GetMarketPricesCompressedRespSimple}
containing slots \code{runners} and \code{prices}.
}
\description{
Functions to retrieve prices on Betfair markets.

getMarketPrices: Retrieve dynamic market data for a given
Market ID.

getMarketPricesCompressed: Retrieve dynamic market data for
a given Market ID in a compressed format. This service
returns the same information as the Get Market Prices
service but returns it in a ~ (tilde) delimited String.
}
\details{
getCompleteMarketPricesCompressed: Retrieve all back and
lay stakes for each price on the exchange for a given
Market ID in a compressed format. The information returned
is similar to the GetDetailAvailableMarketDepth, except it
returns the data for an entire market, rather than just one
selection.
}
\author{
Vitalie Spinu (\email{spinuvit@gmail.com})
}
\references{
\url{http://code.google.com/p/betfairly/},
\url{https://docs.developer.betfair.com/betfair/}
}
\seealso{
\code{\link{betfairly-package}}
\code{\link{bfSimpleOutput-class}}
\code{\link{getActiveEventTypes}}
}

