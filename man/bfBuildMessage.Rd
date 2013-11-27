\name{bfBuildMessage}
\alias{bfBuildMessage}
\title{bfBuildMessage}
\usage{
  bfBuildMessage(operation, parameters = list(), prefix,
    ns, tp, sessionToken = .sessionToken)
}
\arguments{
  \item{operation}{}

  \item{...}{parameter = value pairs}

  \item{parameters}{named list of parameters}

  \item{prefix}{namespace prefix to be used}

  \item{ns}{main namespace url}

  \item{tp}{types namespace url}

  \item{sessionToken}{}
}
\value{
  \code{'\link[=bfSimpleOutput-class]{bfSimpleOutput}'}
  object, xml node or S4 object, as specified by the
  \code{output} parameter
}
\description{
  Build a correctly formated xml betfair request message to
  be send with \code{'\link{bfRequest}'}.
}
\details{
  See \code{betfair:::.bfServers} for list of available
  servers. Used to prepare the message to
  \code{'link{bfRequest}'}.
}
\author{
  Vitalie Spinu (\email{spinuvit@gmail.com})
}
\references{
  \url{https://docs.developer.betfair.com/betfair/}
}
\seealso{
  \code{'\link{betfairly-package}'}
  \code{'\link{bfRequest}'}
}
\keyword{internal}

