\name{bfSimpleOutputList}
\alias{bfSimpleOutputList}
\title{\code{bfSimpleOutputList} is an S4 list containing simple Betfair API output
as familiar basic R types.}
\description{
  Additional slots are usually data frames containing
  complex tabular data. For example an object
  \code{GetEventsRespSimple}, returned by function
  \code{\link{getEvents}}, contains two slots -
  \code{eventItems} and \code{marketItems}. }
  \section{Methods}{ \describe{
  \item{show}{\code{signature(object = "bfSimpleOutput")}:
  ... } }
}
\examples{
getClass("bfSimpleOutputList")
}
\author{
  Vitalie Spinu
}
\seealso{
  \code{\link{betfairly-package}}
  \code{\link{bfInitClasses}}
}
\keyword{class}

