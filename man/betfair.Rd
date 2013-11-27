\docType{class}
\name{betfair}
\alias{betfair}
\alias{betfairly-class}
\title{All betfairly S4 classes inherit from this class.}
\description{
  If \code{'output'} parameter is "S4", betfairly api
  functions return an S4 object. The structure of this
  object is described by the WSDL betfair service file.
}
\details{
  For parsimony reasons the betfairly S4 classes are not
  installed with the package. You need to run
  \code{bfInitClasses()} to initialize the S4 interface.

  Note what the package \code{XMLSchema} from
  \url{www.omegahat.org} is needed for this:

  \code{ install.packages("XMLSchema") ## binaries }

  or

  \code{ install.packages("XMLSchema", repos =
  "http://www.omegahat.org/R", type = "source")}
}
\seealso{
  \code{'\link{betfairly-package}'}
  \code{'link{bfInitClasses}'}
}
\keyword{class}

