\name{getBet}
\alias{getBet}
\alias{getBetHistory}
\alias{getBetLite}
\alias{getBetMatchesLite}
\alias{getMarketProfitAndLoss}
\alias{getMUBets}
\alias{getMUBetsLite}
\title{Access your bets}
\usage{
  getBet(betId, server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getBetLite(betId, server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getBetMatchesLite(betId, server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMUBets(marketId, betIds, betStatus = "MU",
    matchedSince, orderBy = "BET_ID", sortOrder = "ASC",
    recordCount = 200, startRecord = 0,
    excludeLastSecond = FALSE,
    server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMUBetsLite(marketId, betIds, betStatus = "MU",
    matchedSince, orderBy = "BET_ID", sortOrder = "ASC",
    recordCount = 200, startRecord = 0,
    excludeLastSecond = FALSE,
    server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getBetHistory(marketId = 0, eventTypeIds = NULL,
    detailed = FALSE, sortBetsBy = "NONE",
    betTypesIncluded = "S", marketTypesIncluded = "O",
    placedDateFrom = Sys.Date(), placedDateTo = Sys.time(),
    recordCount = 100, startRecord = 0, locale, timezone,
    server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())

  getMarketProfitAndLoss(marketID,
    includeSettledBets = FALSE, includeBspBets = TRUE,
    netOfCommission = FALSE, locale,
    server = getOption("bfServer"),
    output = getOption("bfOutput"), curlOpts = list())
}
\arguments{
  \item{betId}{The unique bet identifier}

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

  \item{betId}{The unique bet identifier}

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

  \item{betId}{The unique bet identifier}

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

  \item{marketId}{For \strong{getMUBets} and
  \strong{getMUBetsLite}: If marketId is present and
  non-zero, then bets placed on the specified market are
  returned and any bet ids specified in betIds are
  ignored.\cr For \strong{getBetHistory} Returns the
  records of your matched or unmatched bets for the
  specified market. If you use this parameter you must not
  specify \code{eventTypeId} array. Note that, if you
  specify a marketId, you must also specify either M or U
  as the value for the \code{betTypesIncluded} parameter.}

  \item{betIds}{Specifies the betId of each bet you want
  returned. The maximum number of bets you can include in
  the array is 200. If you include marketId in the request
  and marketId contains a non-zero value, then betIds is
  ignored. If you specify a betId, then you must specify MU
  for betStatus.}

  \item{betStatus}{M, U or MU. The status of the bets to
  return (matched, unmatched, or both) - please see
  betfairly Simple Data Types . If you specify a betId,
  then you must specify MU.}

  \item{matchedSince}{Specifies a date and time to start
  from for the list of returned bets. Any R date-time
  object or string recognized by as.POSIXlt. Use
  \code{asBFDateTime} to see how your time input is
  interpreted. If you use the matchedSince parameter and
  you have specified a betStatus of MU, the bets returned
  will ignore any limit you set (using recordCount) for the
  number of records to be returned. Specifying a betStatus
  of MU causes the API to return your unmatched bets along
  with the matched ones.}

  \item{orderBy}{The order of returned results. Valid
  orders are BET_ID, PLACED_DATE, and MATCHED_DATE.}

  \item{sortOrder}{ASC or DESC. Whether the results are in
  ascending or descending order}

  \item{recordCount}{Maximum number of records to return.
  The maximum number allowed is 200.}

  \item{startRecord}{The first record number to return
  (supports paging). Record numbering starts from 0. For
  example, to retrieve the third record and higher, set
  startRecord to 2.}

  \item{excludeLastSecond}{If true, the API excludes bets
  placed or matched that occurred less than one second
  before the GetMUBets call. Set this to true if you want
  to ensure that the response does not include bets that
  may have changed state between the time you sent the
  request and before the response was generated. If false,
  all bets are returned. Therefore, you may receive a
  response that indicates an unmatched bet that has
  actually been matched during the time taken for the API
  to respond.}

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

  \item{betIds}{A vector specifying the betId of each bet
  you want returned. The maximum number of bets you can
  include in the array is 200. If you include marketId in
  the request and marketId contains a non-zero value, then
  betIds is ignored. If you specify a betId, then you must
  specify MU for betStatus.}

  \item{betStatus}{}

  \item{matchedSince}{}

  \item{orderBy}{}

  \item{sortOrder}{}

  \item{recordCount}{}

  \item{startRecord}{}

  \item{excludeLastSecond}{}

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

  \item{eventTypeIds}{An array of event types to return.
  For matched and unmatched bets only, you can leave it
  unspecified and specify zero (the default) as the
  marketId to receive records of all your bets on the
  exchange.}

  \item{detailed}{[logical] Whether to show details of all
  the matches on a single bet}

  \item{sortBetsBy}{[ASC, DESC] How the bets are ordered.}

  \item{betTypesIncluded}{[C Cancelled, L Lapsed, M
  Matched, MU Matched and Unmatched, S Settled (default), U
  Unmatched, V Voided] Indicates the status of the bets to
  include in the response. If your \code{betHistory}
  request is for a specific market (in other words, if you
  have specified a marketId in your request), then you must
  specify either M or U as the value for betTypesIncluded.
  Otherwise you will receive an INVALID_BET_STATUS error.
  Only settled markets return cancelled, void, or lapsed
  bets.}

  \item{marketTypesIncluded}{[A Asian Handicap, L Line, O
  Odds (default), R Range] Indicates the types of market
  that you want your betting history returned for.}

  \item{placedDateFrom}{Any R date/date-time object is
  accepted or any character string recognized by
  as.POSIXlt. Default to current day at 00:00.}

  \item{placedDateTo}{Any R date/date-time object. Default
  to Sys.time().}

  \item{recordCount}{The maximum number of records to
  return. This number must be between 1 and 100,
  inclusive.}

  \item{startRecord}{The first record number to return
  (supports paging). Record numbering starts from 0. For
  example, to retrieve the third record and higher, set
  startRecord to 2.}

  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{timezone}{Specify an alternative time-zone from the
  user account default.}

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

  \item{marketID}{The market ID for which the profit and
  loss for the user is to be returned}

  \item{includeSettledBets}{logical If TRUE then the P&L
  calculation for each runner includes any profit and loss
  from any bets on runners that have already been settled.
  The default is FALSE, which matches the default on
  Betfair.com.}

  \item{includeBspBets}{If TRUE, BSP bets are returned as
  part of the P&L}

  \item{netOfCommission}{If TRUE return P&L net of users
  current commission rate for this market including any
  special tariffs, default is FALSE.}

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
}
\value{
  A list of class \code{GetBetRespSimple} containing slot
  \code{matches} with info about matched portions of the
  bet.

  A list of class \code{GetBetLiteRespSimple} with no
  additional slots.  Contains a subset of information from
  data part of \code{getBet} response.

  Data frame of class \code{GetBetMatchesLiteRespSimple}
  containing subset of information from \code{@matches}
  slot in \code{getBet} response.

  Object of class \code{xxx} containing slot

  Object of class \code{GetBetHistoryRespSimple} containing
  slots \code{betHistoryItems} - a data frame with one bet
  per row and \code{matches} - a data frame with all the
  matches if the \code{details} parameter was set to TRUE.

  Object of class \code{GetMarketProfitAndLossRespSimple}
  containing slot \code{annotations} which is a data frame
  with P&L data.
}
\description{
  With \code{getBetHistory}, \code{getMUBets} and
  \code{getMUBetsLite} you access information about all
  your bets.  With getBet, getBetLite and getBetMatchesLite
  you can access detailed information about your specific
  bets.

  This is the lite version of the GetBet service.

  This is a lite version of the GetBet service that returns
  information on matched bets.{

  Retrieve information about all your matched and unmatched
  bets on a particular exchange server.  You should be
  aware that voided bets are not returned by getMUBets.
  Your application should track the number of matched and
  unmatched bets against the number of bets returned by
  getMUBets in order to detect a voided bet.

  This is a lite version of the getMUBets service.

  * Profit and loss for single and multi-winner odds
  markets is implemented however it won't calculate
  worstCaseIfWin nor futureIfWin.
}
\details{
  Retrieve information about a particular bet. Each request
  will retrieve all components of the desired bet.

  You can retrieve Cancelled, Lapsed, and Voided bets from
  only settled markets and these bets are available for a
  maximum of 10 days from the date the market was settled.

  * The calculation for AH markets will include
  worstCaseIfWin but not futureIfWin.
}
\author{
  Vitalie Spinu (\email{spinuvit@gmail.com})

  Vitalie Spinu (\email{spinuvit@gmail.com})

  Vitalie Spinu (\email{spinuvit@gmail.com})

  Vitalie Spinu (\email{spinuvit@gmail.com})

  Vitalie Spinu (\email{spinuvit@gmail.com})
}
\references{
  \url{http://code.google.com/p/betfairly/},
  \url{https://docs.developer.betfair.com/betfair/}

  \url{http://code.google.com/p/betfairly/},
  \url{https://docs.developer.betfair.com/betfair/}

  \url{http://code.google.com/p/betfairly/},
  \url{https://docs.developer.betfair.com/betfair/}
}
\seealso{
  \code{\link{betfairly-package}}
  \code{\link{bfSimpleOutput-class}}

  \code{\link{betfairly-package}}
  \code{\link{bfSimpleOutput-class}}

  \code{\link{betfairly-package}}
  \code{\link{bfSimpleOutput-class}}
}

