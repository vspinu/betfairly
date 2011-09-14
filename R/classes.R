## Classes and objects used in the package

setClassUnion("characterOrLogical", c("character", "logical"))

##' Virtual Class to represent the  simplified output of betfairly functions
##'
##' As described in \code{'\link{betfairly-package}'} functions can return four
##' types of output xml, S4,  list or simplified output of
##' class \code{bfSimpleOutput}.
##'
##' There are two classes what inherit from \code{bfSimpleOutput} -
##' \code{\link{bfSimpleOutputList}} and \code{\link{bfSimpleOutputDF}}. All
##' \code{betfairly} functions return an object which extends one of these two
##' classes. The names of the classes are always constructed by appending
##' "Simple" or "SimpleDF" to the name of native Betfair class. For example the
##' function \code{\link{getEvents}} returns an object of class
##' \code{GetEventsRespSimple} meaning that it is a list inherited from
##' \code{bfSimpleOutputList} and the native Betfair response type  is
##' \code{GetEventsResp}, so you can easily  find the documentation in Betfair
##' API reference guide.  Function \code{\link{getAllMarkets}} return an object
##' of class \code{GetAllMarketsRespSimpleDF} which means that it inherits from
##' \code{\link{bfSimpleOutputDF}} and is a data.frame.
##' }
##' \section{Slots}{
##' \describe{
##'     \item{\code{bfType}:}{Name of Betfair SOAP type.}
##'     \item{\code{errorCode}:}{Error code returned by Betfair api. You should check this first.}
##'     \item{\code{minorErrorCode}:}{Age verification error}
##' }
##'
##' @docType class
##' @export
##' @keywords class
##' @seealso  \code{\link{bfSimpleOutputList-class}}, \code{\link{bfSimpleOutputDF-class}}
##' @examples
##'  getClass("bfSimpleOutput")
setClass("bfSimpleOutput",
         representation = representation(
           bfType = "character",
           errorCode = "characterOrLogical",
           minorErrorCode = "characterOrLogical"),
         prototype = prototype(errorCode = NA, minorErrorCode = NA))


##' \code{bfSimpleOutputDF} is an S4 data.frame containing betfair tabular output.
##'
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfSimpleOutput-class}} \code{\link{bfSimpleOutputList}}
##' @keywords class
##' @author Vitalie Spinu
##' @examples
##'  getClass("bfSimpleOutputDF")
setClass("bfSimpleOutputDF",
         contains = c("data.frame", "bfSimpleOutput"))

## bfListOutput,  bfS4Output, bfXMLOutput?? implement? :tothink


##' \code{bfSimpleOutputList} is an S4 list containing simple Betfair API output
##' as familiar basic R types.
##'
##'
##' Additional slots are usually data frames containing complex tabular
##' data. For example an object \code{GetEventsRespSimple}, returned by function
##' \code{\link{getEvents}},  contains two slots - \code{eventItems} and
##' \code{marketItems}.
##' }
##' \section{Methods}{
##' \describe{
##'     \item{show}{\code{signature(object = "bfSimpleOutput")}: ... }
##' }
##' @export
##' @seealso \code{\link{betfairly-package}} \code{\link{bfInitClasses}}
##' @keywords class
##' @author Vitalie Spinu
##' @examples
##'   getClass("bfSimpleOutputList")
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
    setClass("GetCompleteMarketPricesCompressedRespSimple",
             representation = representation(
               removedRunners = "data.frame",
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
             "PlaceBets", "CancelBets", "CancelBetsByMarket", "UpdateBets", "GetSubscriptionInfo",
             "GetAccountStatement")
    for(nm in dfc)
        setClass(paste(nm, "RespSimpleDF", sep = ""), contains = "bfSimpleOutputDF")
})

##' All betfairly S4 classes inherit from this class.
##'
##' If \code{'output'} parameter is "S4", betfairly api functions return an S4
##' object. The structure of this object is described by the WSDL betfair
##' service file.
##'
##' For parsimony reasons the betfairly S4 classes are not installed with
##' the package. You need to run \code{bfInitClasses()} to initialize the S4 interface.
##'
##' Note what the package \code{XMLSchema}  from \url{www.omegahat.org} is needed for this:
##'
##' \code{   install.packages("XMLSchema")  ## binaries }
##'
##' or
##'
##' \code{   install.packages("XMLSchema", repos = "http://www.omegahat.org/R", type = "source")}
##'
##' @aliases betfairly-class
##' @docType class
##' @seealso \code{'\link{betfairly-package}'} \code{'link{bfInitClasses}'}
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
                  if(length(object@.Data)){
                      str(structure(object@.Data, names = object@names), no.list=T)
                      cat("\n")
                  }
                  for(nm in snames[ordinnames]){
                      cat("+@", nm, ":\n", sep = "")
                      print(slot(object, nm))
                      cat("\n")
                  }
              }
          )
    setMethod("show", "bfSimpleOutputDF",
              function(object){
                  cat("Data.frame of class '", class(object), "' (errorCode - ", object@errorCode, "):\n\n", sep="")
                  snames <- slotNames(object)
                  ordinnames <- !snames %in% c(".Data", ".S3Class", "row.names", "names", "bfType", "errorCode", "minorErrorCode")
                  print(S3Part(object, strict = TRUE))
                  for(nm in snames[ordinnames]){
                      cat("+@", nm, ":\n", sep = "")
                      print(slot(object, nm))
                  }
              }
              )
    setAs("character", "factor", function(from) factor(from))
    setClass("SOAPDateTime", contains = "character")
    ## removeClass("SOAPDateTime")
    setAs("character", "SOAPDateTime", function(from) new("SOAPDateTime", from))
})


