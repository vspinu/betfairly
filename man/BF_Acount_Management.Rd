\name{getAccountFunds}
\alias{>BF_Acount_Management}
\alias{getAccountFunds}
\alias{getAccountStatement}
\alias{getSubscriptionInfo}
\alias{transferFunds}
\alias{viewProfile}
\title{Account management.}
\usage{
getAccountFunds(server = getOption("bfServer"),
  output = getOption("bfOutput"), curlOpts = list())

getAccountStatement(startDate = Sys.Date() - 1, endDate = Sys.time(),
  startRecord = 0, recordCount = 100, itemsIncluded = "ALL", locale,
  ignoreAutoTransfers = TRUE, server = getOption("bfServer"),
  output = getOption("bfOutput"), curlOpts = list())

getSubscriptionInfo(output = getOption("bfOutput"), curlOpts = list())

transferFunds(amount, sourceWalletId = 1, targetWalletId = 2,
  output = getOption("bfOutput"), curlOpts = list())

viewProfile(output = getOption("bfOutput"), curlOpts = list())
}
\arguments{
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

  \item{startDate}{Return records on or after this date.}

  \item{endDate}{Return records on or before this date.}

  \item{startRecord}{The first record number to return
  (supports paging). Record numbering starts from 0. For
  example, to retrieve the third record and higher, set
  startRecord to 2.}

  \item{recordCount}{The maximum number of records to
  return.}

  \item{itemsIncluded}{Determines what type of statements
  items to return.}

  \item{locale}{Specify the language for the reply if you
  want a different language than the account default.}

  \item{ignoreAutoTransfers}{_}

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

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{amount}{_}

  \item{sourceWalletId}{The wallet that you are requesting
  the funds to be transferred from. There are two possible
  wallets: 1 = UK Sports Betting wallet 2 = Australian
  Sports Betting wallet}

  \item{targetWalletId}{The wallet that you are requesting
  the funds to be transferred from.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}

  \item{output}{Indicates the form of the returned value.
  Can be "simple" (default), "xml", "list" or "S4". See
  \code{\link{betfairly-package}}.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}
}
\value{
Object of class \code{GetAccountFundsRespSimple} with no
extra slots.

Data.frame of class \code{GetAccountStatementRespSimpleDF}
with no extra slots.

Object of class \code{getSubscriptionInfo} with no extra
slots.

Object of class \code{TransferFundsRespSimple} with no
extra slots.

Object of class \code{ViewProfileRespSimple} with not extra
slots.
}
\description{
Various functions to access information about your account
and wallets.

getAccountStatement: Obtain information about transactions
involving your local wallet on an exchange server.

getSubscriptionInfo: Return information on your API
subscription.

transferFunds: Transfer funds between your UK and
Australian account wallets. The concept of account wallets
has been introduced in release 5.0 of the Betfair API.
Instead of a single account holding all of a customer's
funds for betting on sports events, there are now two
"wallets" for each customer's account: one for betting on
the UK exchange server and one for betting on the
Australian exchange server.

viewProfile: Retrieve information about the user account,
such as the registered address, e-mail address, phone
numbers, etc.
}
\details{
getAccountFunds: Retrieve information about your local
wallet on a particular exchange server. For an explanation
of the concept of wallets, see "Using Region-specific
Wallets for Placing Bets" on page 12 in Betfair API
Developer Documentation.
}
\author{
Vitalie Spinu (\email{spinuvit@gmail.com})

Vitalie Spinu (\email{spinuvit@gmail.com})
}
\references{
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
}

