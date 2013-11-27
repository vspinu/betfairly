\docType{class}
\name{bfSimpleOutput}
\alias{bfSimpleOutput}
\title{Virtual Class to represent the  simplified output of betfairly functions}
\description{
  As described in \code{'\link{betfairly-package}'}
  functions can return four types of output xml, S4, list
  or simplified output of class \code{bfSimpleOutput}.
}
\details{
  There are two classes what inherit from
  \code{bfSimpleOutput} - \code{\link{bfSimpleOutputList}}
  and \code{\link{bfSimpleOutputDF}}. All \code{betfairly}
  functions return an object which extends one of these two
  classes. The names of the classes are always constructed
  by appending "Simple" or "SimpleDF" to the name of native
  Betfair class. For example the function
  \code{\link{getEvents}} returns an object of class
  \code{GetEventsRespSimple} meaning that it is a list
  inherited from \code{bfSimpleOutputList} and the native
  Betfair response type is \code{GetEventsResp}, so you can
  easily find the documentation in Betfair API reference
  guide.  Function \code{\link{getAllMarkets}} return an
  object of class \code{GetAllMarketsRespSimpleDF} which
  means that it inherits from
  \code{\link{bfSimpleOutputDF}} and is a data.frame. }
  \section{Slots}{ \describe{ \item{\code{bfType}:}{Name of
  Betfair SOAP type.} \item{\code{errorCode}:}{Error code
  returned by Betfair api. You should check this first.}
  \item{\code{minorErrorCode}:}{Age verification error} }
}
\examples{
getClass("bfSimpleOutput")
}
\seealso{
  \code{\link{bfSimpleOutputList-class}},
  \code{\link{bfSimpleOutputDF-class}}
}
\keyword{class}

