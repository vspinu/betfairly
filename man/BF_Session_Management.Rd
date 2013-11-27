\name{bfLogin}
\alias{bfLogin}
\alias{bfLogout}
\alias{bfSessionHandler}
\alias{>BF_Session_Management}
\alias{bfSessionToken}
\alias{keepAlive}
\title{Session Management.}
\usage{
  bfLogin(username, password, productId=82, ipAddress="0",
    locationId=0, vendorSoftwareId=0, curlOpts=list())
    bfLogout(curlOpts = list()) keepAlive(curlOpts =
    list()) bfSessionToken() bfSessionHandler()
}
\arguments{
  \item{username}{The username with which to login to the
  API for a new session.}

  \item{password}{The password with which to login to the
  API for a new session.}

  \item{productId}{The API product ID with which to login
  to the API for a new session. If you want to use the Free
  Access API, use 82. If you are a paying API subscriber,
  use the Id provided when you signed up.}

  \item{ipAddress}{For applications that proxy the user's
  connection, the IP address of the user's computer.
  Betfair may inform you in the future if you need to
  provide this field, otherwise set this field to 0 (the
  default).}

  \item{locationId}{The location ID with which to login for
  a new session.}

  \item{vendorSoftwareId}{The vendor software ID with which
  to login to the API for a new session. This is only
  relevant for software vendors and is provided when
  software vendors sign up.}

  \item{curlOpts}{RCurl options passed directly to
  \code{\link{curlPerform}}. You can also set the defaults
  with \code{options(bfCurlOpts = list(opt1 = val1, opt2 =
  val2, ...))}.}
}
\value{
  invisibly a sessionToken string
}
\description{
  \code{login} enables customers to log in to the API
  service and initiates a secure session for the user.
  Users can have multiple sessions 'alive' at any point in
  time.\cr\cr \code{logout} allows you to explicitly end
  your session.\cr\cr \code{keepAlive} can be used to stop
  a session timing out. Every call to the Betfair API
  returns a token, in the sessionToken field, that
  identifies a login session. Every time your application
  calls the Betfair API and is returned a sessionToken, the
  session timeout is reset to approximately 20 minutes.
  After the timeout has passed, the session is expired and
  you need to login again.\cr If you want to keep your
  login session active, but your application has not made
  any Betfair API calls that would generate a new
  sessionToken and reset the session timeout, you can call
  keepAlive to obtain a new sessionToken and reset the
  session timeout.\cr\cr \code{bfSessionTocken} returns the
  current session token, if any, NULL otherwise.
}
\details{
  \code{bfSessionHandler} (not implemented yet) creates a
  session handler used to access multiple sessions. A
  betfair functions can be accessed through
  \code{obj$foo(...)}, where \code{obj} is the session
  handler object returned by \code{bfSessionHandler}. All
  the functions in the handler share common parent
  environment where the \code{.sessionToken} is stored.

  Every betfair API request/response must contains a
  session token.  All functions in betfairly-package store
  the session token in .GlobalEnv in .sessionToken variable
  by \code{<<-} assignment. To manage a single account this
  is appropriate. To manipulate several different sessions
  at the same time create a handler for each session with
  \code{bfSessionHandler}.
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

