\name{bfInitClasses}
\alias{bfInitClasses}
\title{Initialize betfair S4 interface}
\usage{
bfInitClasses(verbose = FALSE, where = .GlobalEnv)
}
\arguments{
  \item{verbose}{Print info message for each class}

  \item{where}{Environment in which to store the class
  definitions; defaults to global environment.}
}
\value{
\code{'\link[=bfSimpleOutput-class]{bfSimpleOutput}'}
object, xml node or S4 object, as specified by the
\code{output} parameter
}
\description{
All betfair functions are capable of producing a valid S4
object corresponding to betfair SOAP specification. \cr For
parsimony reasons the betfair S4 classes are not installed
with the package. You need to run \code{ bfInitClasses()}
to initialize them. Note that \code{XMLSchema} package is
required for this initialization.
}
\examples{
\dontrun{
install.packages("XMLSchema", repos = "http://www.omegahat.org/R")  ## windows binaries?
install.packages("XMLSchema", repos = "http://www.omegahat.org/R", type = "source")  ## from source
bfInitClasses()
}
}
\author{
Vitalie Spinu (\email{spinuvit@gmail.com})
}
\references{
\url{https://docs.developer.betfair.com/betfair/}
}
\seealso{
\code{'\link{betfairly-package}'}
\code{'\link{bfSimpleOutput-class}'}
}

