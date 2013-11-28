\name{bfBet}
\alias{>BF_Bet_Placement}
\alias{bfBet}
\alias{bfBetUpdate}
\alias{cancelBets}
\alias{cancelBetsByMarket}
\alias{placeBets}
\alias{updateBets}
\title{Functions to place, update and cancel bets.}
\usage{
bfBet(marketId, selectionId, price, size, betType = "B", bspLiability = 0,
  betCategoryType = "E", betPersistenceType = "NONE",
  server = getOption("bfServer"))

placeBets(bets = list(), marketIds, selectionIds, prices, sizes,
  betTypes = "B", bspLiabilities = 0, betCategoryTypes = "E",
  betPersistenceTypes = "NONE", server = getOption("bfServer"),
  output = getOption("bfOutput"), curlOpts = list())

cancelBets(bets, server = getOption("bfServer"),
  output = getOption("bfOutput"), curlOpts = list())

cancelBetsByMarket(markets, server = getOption("bfServer"),
  output = getOption("bfOutput"), curlOpts = list())

bfBetUpdate(betId, newPrice, oldPrice, newSize, oldSize, newBetPersistenceType,
  oldBetPersistenceType)

updateBets(bets = list(), betIds, newPrices, oldPrices, newSizes, oldSizes,
  newBetPersistenceTypes, oldBetPersistenceTypes,
  server = getOption("bfServer"), output = getOption("bfOutput"),
  curlOpts = list())
}
\arguments{
  \item{marketId}{Integer specifying the market ID.}

  \item{selectionId}{ID of the desired runner or selection
  within the market}

  \item{price}{numeric The price (odds) you want to set for
  the bet. Valid values are 1.01 to 1000. For a BSP Limit
  on Close bet, specify the desired price limit. For a Back
  bet, the minimum price you want. If the Starting Price is
  lower than this amount, then your bet is not matched. For
  a Lay bet, the maximum acceptable price. If the Starting
  Price is higher than this amount, then your bet is not
  matched. If the specified limit is equal to the starting
  price, then it may be matched, partially matched, or may
  not be matched at all, depending on how much is needed to
  balance all bets against each other (MOC, LOC and normal
  exchange bets).}

  \item{size}{numeric The stake (amount) for an exchange
  bet. The minimum amount for a back bet is 2 (or
  equivalent). If the betPersistenceType is set to SP, then
  the minimum amount for a lay bet is 10 (or equivalent),
  otherwise, the minumum lay bet amount is 2 (or
  equivalent).}

  \item{betType}{'B' - back, 'L'- lay. See details.}

  \item{bspLiability}{numeric This is the maximum amount of
  money you want to risk for a BSP bet. The minimum amount
  for a back bet is 2 (or equivalent). The minimum amount
  for a lay bet is 10 (or equivalent) For a back bet, this
  is equivalent to the stake on a normal exchange bet. For
  a lay bet, this is the equivalent to the liability on a
  normal exchange bet. If after the market is reconciled,
  the actual stake is calculated once the price is known.}

  \item{server}{"GB" (default) or "AU" - a Betfair exchange
  server to use. You can set the default with
  \code{options(bfServer = "AU")}.}

  \item{betCategoryType}{E, M or L. 'E' - Exchange bet, 'M'
  - Market on Close SP bet, 'L' - Limit on Close SP bet. If
  you specify Limit on Close, specify the desired limit
  using the price argument. See details.}

  \item{betPersistenceType}{NONE, IP or SP. Specify what
  happens to an unmatched (or partially unmatched) exchange
  bet when the market turns in-play. If betCategoryType is
  an SP bet, betPersistenceType must be set to NONE. See
  details.}

  \item{bets}{For \emph{\code{placeBets}} an \code{bfBet}
  object or a list (of max 60) such objects.\cr For
  \emph{\code{cancelBets}} a vector of bet ids to be
  canceled (max 40).\cr For \emph{\code{updateBets}} an
  \code{bfBetUpdate} object or a list (of max 15) such
  objects.}

  \item{marketIds}{Vector of integers specifying the market
  IDs.}

  \item{selectionIds}{_}

  \item{prices}{_}

  \item{sizes}{_}

  \item{betTypes}{_}

  \item{bspLiabilities}{_}

  \item{betCategoryTypes}{_}

  \item{betPersistenceTypes}{_}

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

  \item{bets}{}

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

  \item{markets}{Vector of market IDs.}

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

  \item{betId}{The unique identifier for the bet.}

  \item{newPrice}{New odds desired on the bet For BSP Limit
  on Close bets, newPrice should be set to the new limit
  desired. For BSP Limit on Close back bets, you can only
  change the limit downward. For BSP Limit on Close lay
  bets, you can only change the limit upward.}

  \item{oldPrice}{For an exchange bet, original odds on the
  bet.}

  \item{newSize}{New stake desired on the bet}

  \item{oldSize}{For an exchange bet, original stake on the
  bet}

  \item{newBetPersistenceType}{New persistence type on an
  exchange bet. Only valid before the market turns
  in-play.}

  \item{oldBetPersistenceType}{Original persistence type on
  an exchange bet. Only valid before the market turns
  in-play.}

  \item{bets}{Can be an \code{bfBetUpdate} object or a list
  (of max 15) such objects.}

  \item{betIds}{_}

  \item{newPrices}{_}

  \item{oldPrices}{_}

  \item{newSizes}{_}

  \item{oldSizes}{_}

  \item{newBetPersistenceTypes}{_}

  \item{oldBetPersistenceTypes}{_}

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
String of class \code{bfBet}.

Data frame with info on the success of placed bets, one bet
per row.

Data frame with info on canceled bets, one bet per row.

Object of class \code{xxx} containing slot

String of class \code{bfBetUpdate}.

Data frame with info on the success of bet updates, one bet
per row.
}
\description{
Place, update and cancel multiple bets at a time. \cr \cr
For \code{placeBets} and \code{updateBets}, there are two
equivalent ways of supplying the bet info. First, by
passing a list of corresponding objects (\code{bfBet} and
\code{bfBetUpdate}) as \code{bets} argument. Second, a
vectorized (\code{mapplyish}) way, by supplying vectors to
corresponding arguments. Vectorized arguments are recycled
to the same length if needed. These arguments are all in
plural and are not documented below.

\describe{

cancelBets: Cancel multiple unmatched (1 to 40) bets placed
on a single Market. The success or failure of the
individual bet cancellation operation will be indicated by
the Success Boolean. If a portion of the original bet is
already matched, cancelBets cancels the unmatched portion
of the bet.

cancelBetsByMarket: [payed] Cancel all unmatched bets (or
unmatched portions of bets) placed on one or more Markets.
You might use this service to quickly close out a position
on a market.

bfBetUpdate: Constructor of \code{bfBetUpdate} object. You
supply a list of these objects as \code{bets} argument to
\code{cancelBets}.

updateBets: Edit multiple (1 to 15) bets on a single
Market. The success or failure of the individual bet
editing operation is indicated by the Success Boolean. If
newPrice and newSize are both specified the newSize value
is ignored. For example, an original bet is placed for 100
with odds of 1.5: UpdateBets is called with newSize = 200,
newPrice = 2. The original bet is cancelled and a new bet
is place for 100 with odds of 2.
}
\details{
bfBet: Constructor of \code{bfBet} object. You supply a
list of these objects as \code{bets} argument to
\code{placeBets}.

The required fields in bets are dependent on the category
of bet. The following table shows the required fields for
each bet category.

Table 1. Valid Bet Category request field combinations
\tabular{lrrrr}{ Bet Category \tab Price \tab Size \tab
BspLiability \tab BetPersistenceType \cr Exchange \tab Yes
\tab Yes \tab No \tab Yes \cr Market on Close \tab No \tab
No \tab Yes \tab No \cr Limit on Close \tab Yes \tab No
\tab Yes \tab No }

\item{Bet Types}{

You can specify, for each bet, if you want to place a Back
bet or a Lay bet.

* B - Back bets win when the selection is settled as the
winner in the market. * L - Lay bets win when the selection
is settled as a looser in the market.

For more information on Bet types, see the Betfair website
help. } \item{Bet Categories}{

You can specify, for each bet, whether the bet is a regular
exchange bet, or a Betfair Market on Close (or Starting
Price) bet (with or without a price limit).

* E - Exchange bets are placed on the market and are
matched against bets at the specified or better price.
Exchange bets are matched on a first in, first matched
basis.

* M - Market on Close (MOC) bets remain unmatched until the
market is reconciled. They are matched and settled at a
price that is representative of the market at the point the
market is turned in-play. The market is reconciled to find
a starting price and MOC bets are settled at whatever
starting price is returned. MOC bets are always matched and
settled, unless a starting price is not available for the
selection. Market on Close bets can only be placed before
the starting price is determined.

* L - Limit on Close (LOC) bets are matched if, and only
if, the returned starting price is better than a specified
price. In the case of back bets, LOC bets are matched if
the calculated starting price is greater than the specified
price. In the case of lay bets, LOC bets are matched if the
starting price is less than the specified price. If the
specified limit is equal to the starting price, then it may
be matched, partially matched, or may not be matched at
all, depending on how much is needed to balance all bets
against each other (MOC, LOC and normal exchange bets) }
\item{Bet Persistence}{

You can specify what happens to an Exchange bet that is
unmatched when the market is reconciled and the starting
price is calculated.

* NONE - The unmatched bet is cancelled when the market is
reconciled and turned in-play.

* IP - The unmatched bet stays as an unmatched bet when the
market is turn in-play.

* SP - The unmatched bet becomes a Market on Close bet and
is matched at the starting price.

}}
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
}

