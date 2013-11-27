\name{fromBFXML}
\alias{fromBFXML}
\title{Convert an XML object into S4 object.}
\arguments{
  \item{node}{XML node (as one returned by
  \code{bfRequest})}

  \item{obj}{See details}

  \item{type}{betfair type (equivalently S4 class name) of
  the output}

  \item{converters}{a list of functions to be used to
  convert undefined classes (see the object
  \code{defaultStructBFConverters})}

  \item{forceList}{TRUE for recursive list mirroring the
  node}
}
\description{
  Methods for function \code{fromBFXML}.
}
\details{
  The function \code{fromBFXML} is used internally to
  convert xml nodes resulted from betfair API requests into
  valid S4 objects as specified by betfair service WSDL
  description.  Methods are dispatched only for the second
  argument \code{obj}.

  S4 equivalents for betfair types are not provided with
  the package. You need to initialize them with

  \code{ bfInitClasses()}

  You need \code{XMLSchema} package for this.

  Once installed, you can use \code{str(getClass(foo))} to
  view the strucutre of class \code{'foo'}.

  } \section{\strong{Methods}}{ \describe{

  \item{\code{signature(obj = "ANY")}}{ \code{obj} should
  be a valid S4 object with the same structure as node. It
  is usually created with \code{new(objClass)}, where
  \code{objClass} is the class generated with the
  \code{defineClasses} function from \code{XMLSchema}
  package, or \code{'SSOAP'} package function
  \code{\link[SSOAP]{genSOAPClientInterface}}.

  The function ties hard to find and install all the
  \code{"as"} methods. In rare cases it will fail. To make
  it work you have to define the specific conversion
  yourself.

  \code{'type'} argument is ignored in this method.

  }

  \item{\code{signature(obj = "list")}}{ All elements of
  the \code{node} are converted in corresponding elements
  of a list.  \code{'type'} argument is ignored.  }

  \item{\code{signature(obj = "missing")}}{ Target S4 class
  is taken from \code{'type'} argument, or if missing is
  inferred from the node itself.  }

  \item{\code{signature(obj = "vector")}}{ Like the
  \code{list} method but unlist it's argument, to make it a
  vector.  } }
}
\author{
  Vitalie Spinu
}
\references{
  https://docs.developer.betfair.com/betfair/
}
\seealso{
  \code{'\link{betfairly-package}'}
  \code{'\link{bfInitClasses}'}
}
\keyword{internal}
\keyword{methods}

