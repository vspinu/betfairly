\name{bfRequest}
\alias{bfRequest}
\title{bfRequest}
\usage{
bfRequest(mess, action, header,
  url = "https://api.betfair.com/global/v3/BFGlobalService",
  curlOpts = list())
}
\arguments{
  \item{mess}{A correctly formated xml message (as returned
  by bfBuildMessage)}

  \item{action}{Name of an betfair API function}

  \item{header}{an optional httpheader}

  \item{url}{service url}

  \item{curlOpts}{Rcurl options, see
  \code{'\link{curlPerform}'} for details}
}
\value{
\code{'\link[=bfSimpleOutput-class]{bfSimpleOutput}'}
object, xml node or S4 object, as specified by the
\code{output} parameter
}
\description{
Send the request to betfair and checks for resulting
errors.
}
\details{
Used in \code{.bfRequestInternal}.
}
\author{
Vitalie Spinu (\email{spinuvit@gmail.com})
}
\references{
\url{https://docs.developer.betfair.com/betfair/}
}
\seealso{
\code{'\link{betfairly-package}'}
\code{'\link{bfBuildMessage}'}
}
\keyword{internal}

