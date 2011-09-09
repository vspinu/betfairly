## Classes and objects used in the package

setClassUnion("characterOrLogical", c("character", "logical"))

##' Virtual Class to represent the  output of betfair functions
##'
##' @docType class
##' @export
##' @keywords internal class
setClass("bfSimpleOutput",
         representation = representation(
           bfType = "character",
           errorCode = "characterOrLogical",
           minorErrorCode = "characterOrLogical"))

eval({
    setClass("bfSimpleOutputDF", contains = c("data.frame", "bfSimpleOutput"))
})

## bfListOutput,  bfS4Output, bfXMLOutput?? implement? :tothink

##' Class to represent the simplified output of betfair functions.
##'
##' As described in \code{'\link{betfair-package}'} functions can have three types of output xml, S4 and simplified
##' output which is a data frame or an object of class bfSimpleOutput.
##'
##' \code{bfSimpleOutput} is an S4 list containing complex  Betfair API output
##' as familiar R objects, usually one or two data.frames. Normally  this is the main
##' output of the function and that the user is interested in.
##'
##' All other (usually simple) information which comes with Betfair API response
##' is stored in \code{info} slot.
##'
##' }
##' \section{Slots}{
##' \describe{
##'     \item{\code{.Data}:}{Object of class \code{"list"}.}
##'     \item{\code{bfType}:}{Object of class \code{"character"} This is the native betfair class. You can see the S4 representation of it by \code{getClass([bfType])}. See \code{\link{bfInitClasses}} for more details.}
##'     \item{\code{info}:}{Object of class \code{"list"}. This is a list of objects of basic types returned by the API and usually constitutes the information fields of API response.}
##'     \item{\code{names}:}{Object of class \code{"character"}.}
##' }
##' }
##' \section{Extends}{
##'
##' Class \code{"\linkS4class{namedList}"}, directly.\
##'
##' Class \code{"\linkS4class{list}"}, by class "namedList", distance 2.
##'
##' Class \code{"\linkS4class{vector}"}, by class "namedList", distance 3.
##'
##' Class \code{"\linkS4class{SOAPTypeOrList}"}, by class "namedList", distance 3.
##'
##' }
##' \section{Methods}{
##' \describe{
##'     \item{show}{\code{signature(object = "bfSimpleOutput")}: ... }
##' }
##' @export
##' @seealso \code{\link{betfair-package}} \code{\link{bfInitClasses}}
##' @keywords class
##' @author Vitalie Spinu
setClass("bfSimpleOutputList",
         contains = c("namedList", "bfSimpleOutput"))

eval({
    setClass("GetEventsRespSimple",
             representation = representation(
               eventItems = "data.frame",
               marketItems = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetMarketRespSimple",
             representation = representation(
               runners = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetMarketInfoRespSimple",
             contains = "bfSimpleOutputList")
    setClass("GetMarketTradedVolumeRespSimple",
             representation = representation(
               priceItems = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetMarketTradedVolumeCompressedRespSimple",
             representation = representation(
               runners = "data.frame",
               volumes = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetMarketPricesCompressedRespSimple",
             representation = representation(
               runners = "data.frame",
               prices = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetMarketPricesRespSimple",
             representation = representation(
               runnerPrices = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetBetRespSimple",
             representation = representation(
               matches = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetBetHistoryRespSimple",
             representation = representation(
               matches = "data.frame",
               betHistoryItems = "data.frame"),
             contains = "bfSimpleOutputList")
    setClass("GetBetLiteRespSimple",
             contains = "bfSimpleOutputList")
    setClass("GetAccountFundsRespSimple",
             contains = "bfSimpleOutputList")
    setClass("TransferFundsRespSimple",
             contains = "bfSimpleOutputList")
    setClass("ViewProfileRespSimple",
             contains = "bfSimpleOutputList")
    setClass("GetMarketProfitAndLossRespSimple",
             representation = representation(
               annotations = "data.frame"),
             contains = "bfSimpleOutputList")

    ## DF classes
    dfc <- c("GetEventTypes", "GetAllMarkets", "GetMUBets", "GetMUBetsLite",
             "PlaceBets", "CancelBets", "CancelBetsByMarket", "UpdateBets", "GetSubscriptionInfo")
    for(nm in dfc)
        setClass(paste(nm, "RespSimpleDF", sep = ""), contains = "bfSimpleOutputDF")
})

##' All betfair S4 classes inherit from this class
##'
##' If \code{'output'} parameter is "S4", betfair api functions return an S4
##' object. The structure of this object is described by the WSDL betfair
##' service file.
##'
##' For parsimony reasons the betfair S4 classes are not installed with
##' the package. You need to run
##'
##' \code{   bfInitClasses()}
##'
##' to initialize the S4 interface.
##'
##' Also you will need \code{XMLSchema} package from \url{www.omegahat.org}:
##'
##' \code{   install.packages("XMLSchema")  ## windows binaries }
##'
##' \code{   install.packages("XMLSchema", repos = "http://www.omegahat.org/R", type = "source")  ## from source}
##' @aliases betfair-class
##' @docType class
##' @seealso \code{'\link{betfair-package}'} \code{'link{bfInitClasses}'}
##' @export
##' @keywords class
setClass("betfair")


eval({
    ## silent roxygen and prohibit creating docs for methods
    setMethod("show", "betfair", function(object) str(object))
    setMethod("show", "bfSimpleOutputList",
              function(object){
                  cat("List of class '", class(object), "' (errorCode - ", object@errorCode, "):\n\n", sep="")
                  snames <- slotNames(object)
                  ordinnames <- !snames %in% c(".Data", "names", "bfType", "errorCode", "minorErrorCode")
                  if(length(object@.Data))
                      str(structure(object@.Data, names = object@names), no.list=T)
                  for(nm in snames[ordinnames]){
                      cat("\n+@", nm, ":\n", sep = "")
                      print(slot(object, nm))
                  }
              }
          )
    setMethod("show", "bfSimpleOutputDF",
              function(object){
                  cat("Data frame of class '", class(object), "' (errorCode - ", object@errorCode, "):\n\n", sep="")
                  snames <- slotNames(object)
                  ordinnames <- !snames %in% c(".Data", ".S3Class", "row.names", "names", "bfType", "errorCode", "minorErrorCode")
                  print(S3Part(object, strict = TRUE))
                  for(nm in snames[ordinnames]){
                      cat("\n+@", nm, ":\n", sep = "")
                      print(slot(object, nm))
                  }
              }
              )
    setAs("character", "factor", function(from) factor(from))
    setClass("SOAPDateTime", contains = "character")
    ## removeClass("SOAPDateTime")
    setAs("character", "SOAPDateTime", function(from) new("SOAPDateTime", from))
})


