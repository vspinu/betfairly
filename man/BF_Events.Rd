\name{getAllEventTypes}
\alias{>BF_Events}
\alias{getActiveEventTypes}
\alias{getAllEventTypes}
\alias{getEvents}
\title{Betfair Events}
\usage{
  getAllEventTypes(locale, output = getOption("bfOutput"),
    curlOpts = list())

  getActiveEventTypes(locale,
    output = getOption("bfOutput"), curlOpts = list())

  getEvents(eventParentId = 1L, locale,
    output = getOption("bfOutput"), curlOpts = list())
}
\arguments{
  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{eventParentId}{integer This is either an Id value
  for a single item (in an array of eventTypeItems returned
  by GetAllEventTypes or GetActiveEventTypes), or it is an
  eventId for a single eventItem (in an array of eventItems
  returned by an earlier GetEvents request).}

  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}
}
\value{
  A data frame with columns id nextMarketId and exchangeId;
  an xml node or S4 object, as specified by the
  \code{output} parameter

  A data frame with columns id, nextMarketId and
  exchangeId; an xml node or an S4 object, as specified by
  the \code{output} parameter

  Object of class \code{GetEventsRespSimple} which inherits
  from \code{\link[=bfSimpleOutput-class]{bfSimpleOutput}}
  class. With slots \code{eventItems} and
  \code{marketItems} which are data.frames.
}
\description{
  Functions to retrieve betfair events (Games, sports,
  politics etc)

  Allows the customer to retrieve lists of all categories
  of sporting events (Games, Event Types) that are
  available to bet on: in other words, all those that have
  at least one currently active or suspended market
  associated with them. This means, therefore, that the
  service would, for example, always return the event types
  Soccer and Horse Racing but would not return Olympics
  2004 or EURO 2004 after those events had finished.

  From API 5.0 onwards, the GetEvents service returns
  details of line and range markets, where these markets
  are available for an event.  Requests for the GetEvents
  service take as input a parameter called eventParentID.
  The value of this parameter is either: the (integer) Id
  value from one item in an array of eventTypeItems that
  has been returned by the GetAllEventTypes or
  GetActiveEventTypes services; or an (integer) eventId
  value from one item in an array of eventItems that has
  been returned by an earlier GetEvents request.  Use the
  GetEvents service repeatedly, specifying a different
  value for eventParentId in each request, until there are
  no further events to request (this means you have reached
  the leafnode of the branch of the events tree you have
  been navigating). To retrieve full details of a betting
  market whose details have been returned by the GetEvents
  service, you need to send a GetMarket request to the
  exchange server indicated by the market's exchangeId
  parameter (see CROSS REFERENCE TEXT NEEDS RESOLVING).
  This GetMarket request must also specify the marketId for
  the market you are requesting. Both the exchangeId and
  the marketId are returned by GetEvents. For information
  about GetMarket, see Chapter 24 .
}
\details{
  Allows the customer to retrieve lists of all categories
  of sports (Games, Event Types) that have at least one
  market associated with them, regardless of whether that
  market is now closed for betting. This means that, for
  example, the service would always return the event types
  Soccer and Horse Racing and would also return Olympics
  2004 or EURO 2004 for a certain period after the markets
  for those events had closed; it would also return
  Olympics 2004 or EURO 2004 for a certain period before
  the markets for those events had opened. The service
  returns information on future events to allow API
  programmers to see the range of events that will be
  available to bet on in the near future.
}
\note{
  The GetActiveEventTypes service is a global service, and
  it returns information about the events available on both
  the UK and the Australian exchange servers.
}
\author{
  Vitalie Spinu (\email{spinuvit@gmail.com})

  Vitalie Spinu (\email{spinuvit@gmail.com})

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

