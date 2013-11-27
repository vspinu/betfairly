\docType{package}
\name{betfairly-package}
\alias{betfairly-package}
\title{Access Betfair API from R}
\description{
  The \code{betfairly} package allows to access most of the
  Betfair
  \href{https://docs.developer.betfair.com/betfair/}{API}
  directly from R.
}
\details{
  For the list of all implemented functions and the details
  of the current development status please see
  \href{https://code.google.com/p/betfairly/source/browse/trunk/inst/extra/todo.org}{todo}.

  If a particular functionality that you need is missing,
  please contact the author.

  For examples of usage see
  \href{https://code.google.com/p/betfairly/source/browse/trunk/inst/extra/examples.R}{here}.

  \bold{Table of most common functions:}

  \tabular{ll}{ \bold{If you want to:} \tab \bold{Use:}\cr
  Login \tab \code{\link{bfLogin}}\cr Request a list of
  available events \tab \code{\link{getActiveEventTypes}}
  \cr Request a list of Market for a specific eventType
  \tab \code{\link{getAllMarkets}} \cr Request details of a
  Market (excluding prices) \tab \code{\link{getMarket}}
  \cr Request prices for a Market\tab
  \code{\link{getMarketPricesCompressed}}\cr Place a bet
  \tab \code{\link{placeBets}}\cr Cancel a bet before it is
  matched\tab \code{\link{cancelBets}}\cr Retrieve a list
  of my Matched/Unmatched bets\tab
  \code{\link{getMUBets}}\cr Edit an Unmatched bet \tab
  \code{\link{updateBets}}\cr Retrieve the P&L for a market
  \tab \code{\link{getMarketProfitAndLoss}}\cr Place a
  Betfair SP bet \tab \code{\link{placeBets}}\cr Check if a
  market is in-play now \tab
  \code{\link{getMarketPricesCompressed}}\cr Check if a
  market is due to be turned in-play\tab
  \code{\link{getAllMarkets}}\cr Retrieve a list of Settled
  bets \tab \code{\link{getBetHistory}}\cr Retrieve your
  P&L for a market \tab
  \code{\link{getMarketProfitAndLoss}} }

  For a description of payed and free access types see
  \url{http://bdp.betfair.com/index.php?option=com_content&task=view&id=36&Itemid=64}.
  }

  \section{Output of \code{betfairly} functions}{

  All \code{betfairly} API functions can return four types
  of output, given by the \code{option} parameter which can
  be:

  \describe{

  \item{\code{simple} (the default)}{Simplified output
  represented by a \code{\link{bfSimpleOutput}} object
  containing slots \code{bfType} (original betfair class),
  \code{errorCode} ("OK" if succeed), and
  \code{minorErrorCode} (usually an empty string). See
  \code{\link{bfSimpleOutput-class}} for more information.}

  \item{\code{xml}}{raw XML representation}

  \item{\code{list}}{recursive list mirroring the structure
  of the node}

  \item{\code{S4}}{S4 object as described by the service
  SOAP protocol. Note what you will need \code{XMLSchema}
  package for the S4 conversion to work, as it defines some
  classes which are not provided with \code{betfairly}
  package. See \code{\link{bfInitClasses}} for further
  instructions. }

  }

  You can change the default by setting the \code{bfOutput}
  option:

  \code{options(bfOutput = "XML")}

  }

  \section{Betfair exchange servers}{ Functions to betfair
  exchange services accept a \code{server} parameter, which
  can be either "GB" (the default) or "AU". You can set a
  different default with \code{options(bfServer = "AU")} }

  \section{Curl Options}{ Each \code{betfairly} function
  accepts \code{curlOpts} parameter which is passed
  directly to \code{\link{curlPerform}}, see the
  documentation of that function for details. }

  \section{Reporting Bugs}{Before reporting bugs please see
  the relevant section in the official Betfair
  \href{https://docs.developer.betfair.com/betfair/}{documentation}
  and ensure it's not a betfair service issue. Known
  isssues are documented for each Betfair API action.}

  \section{Disclaimer}{ The betfairly package is provided
  with absolutely no warranty. The documentation of the
  functional API is an adapted and abbreviated version of
  official Betfair
  \href{https://docs.developer.betfair.com/betfair/}{documentation}.
  Please refer to it for the complete reference.
}
\author{
  Vitalie Spinu \email{spinuvit@gmail.com}
}
\references{
  \url{http://code.google.com/p/betfairly/},
  \url{https://docs.developer.betfair.com/betfair/}\cr
  \href{https://bdp.betfair.com/index.php?option=com_weblinks&task=view&catid=59&id=29}{Betfair
  API Quick Start}
}
\seealso{
  \code{\link{bfSimpleOutput-class}}
}
\keyword{api}
\keyword{betfair}
\keyword{package}

