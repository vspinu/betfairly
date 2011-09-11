
#### UTILITIES:

is1NA <- function(x) is.logical(x) && length(x) == 1L && is.na(x)

isXMLStructure <- function(x){
    while(xmlSize(x) == 1L){
        x <- x[[1L]]
    }
    if(xmlSize(x)) TRUE else FALSE
}

.validateCall <- function(call, bfClass){
    classes <- getSlots(bfClass)
    nms <- names(call)[-1]
    nms <- nms[!nms %in% c("output", "curlOpts")]
    for(nm in nms)
        call[[nm]] <- new(classes[[nm]], eval(call[[nm]]))
    call
}

bfSplit <- function(x, split = "~"){
    tsub <- "@!@"
    x <- gsub(paste("\\", split, sep = ""), tsub, x, fixed = T)
    x <- unlist(strsplit(x, split, fixed = T))
    gsub(tsub, paste("\\", split, sep = ""), x, fixed = T)
}

isHTTPError <- function(response){
    ## taken from SSOAP
    if (length(response) == 0L)
        stop("HTTP response had no content")
    !(response[["status"]] %in% c(100L, 200:206))
}

parseSOAP <- function(xmlSource){
    ## taken from SSOAP
    node <- xmlRoot(xmlParse(xmlSource, asText = TRUE, fullNamespaceInfo = TRUE))
    if (xmlName(node) == "Envelope")
        node <- node[[1]]
    if (xmlName(node) == "Body")
        node <- node[[1]]
    node
}

getReqOrRespNames <- function(Class){
    out <- slotNames(getClass(Class))
    out <- out[!out %in% c("header")]
    cat(paste(out, " =, ", sep = ""))
    invisible(out)
}

#### REQUEST:
##' Send the request to betfair and checks for resulting errors.
##'
##' Used in \code{.bfRequestInternal}.
##' @title  bfRequest
##' @param mess A correctly formated xml message (as returned by bfBuildMessage)
##' @param action Name of an betfair API function
##' @param header an optional httpheader
##' @param url service url
##' @param curlOpts Rcurl options,  see \code{'\link{curlPerform}'} for details
##' @return \code{'\link[=bfSimpleOutput-class]{bfSimpleOutput}'} object, xml node or S4 object,  as specified by the \code{output} parameter
##' @seealso \code{'\link{betfairly-package}'} \code{'\link{bfBuildMessage}'}
##' @references \url{https://docs.developer.betfair.com/betfair/}
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @keywords internal
bfRequest <- function(mess, action, header,
                      url = "https://api.betfair.com/global/v3/BFGlobalService",
                      curlOpts = list()){
    if(missing(header))
        header <- structure(c("text/xml", "multipart/*", "text/xml; charset=utf-8",  sprintf("\"%s\"", action)),
                            .Names = c("Accept", "Accept", "Content-Type", "SOAPAction"))
    headerDataFun <- basicTextGatherer()
    bodyDataFun = basicTextGatherer()
    cOpts <-
        if(length(curlOpts)){
            cOpts <- getOption("bfCurlOpts")
            cOpts[names(curlOpts)] <- curlOpts
        }else getOption("bfCurlOpts")
    ## cat(mess, "\n####### ------ ###### \n")
    curlPerform(postfields = mess,
                writeFunction = bodyDataFun$update,
                headerFunction = headerDataFun$update,
                httpheader = header,
                url = url, .opts = cOpts)
    content <- bodyDataFun$value()
    header = RCurl:::parseHTTPHeader(headerDataFun$value(NULL))
    if (isHTTPError(header)){
        fault <- parseSOAP(header)
        ## fault <- SSOAP:::SOAPFault(SSOAP:::parseSOAP(content, asText = TRUE))
        e = simpleError(paste("Error occurred in the HTTP request: ",
          fault@message, xmlValue(fault@detail)))
        httpError = RCurl:::getHTTPErrorClass(header[["status"]])
        class(e) = c("SOAPError", httpError, class(e))
        print(header)
        stop(e)
    }
    ret_node <- parseSOAP(content)
    serv_err <- xmlValue(ret_node[[1L]][["errorCode"]])
    err <- xmlValue(ret_node[[1L]][["header"]][["errorCode"]])
    .sessionToken <<- xmlValue(ret_node[[1L]][["header"]][["sessionToken"]])
    if(err != "OK"){
        e <- simpleError(paste("BF API error occurred: ", serv_err, " - ", err), match.call())
        e$bfAPIError <- err
        e$bfServiceError <- serv_err
        class(e) <- c("bfError", class(e))
        stop(e)
    }
    ## if(!is.na(serv_err) && serv_err != "OK"){
    ##     warning("BF service error occurred: ", serv_err)
    ##     ## if(serv_err == "NO_RESULTS" && err == "OK")
    ##     ##     return(NULL)
    ## }
    ## print(ret_node)
    ## cat("######  responce end ####\n")
    ret_node[[1L]]
}

bfCollapseParams <- function(parameters = list()){
    ##internal function to collapse parameters for betfair request
    names <- names(parameters)
    if(length(parameters)){
        if(is.null(names) || !all(nzchar(names)))
            stop("All parameters must be named")
        paste(unlist(lapply(names, function(nm) paste("<", nm, ">", parameters[[nm]], "</", nm, ">", sep = ""))), collapse = "\n")
    }else "\n"
}

##' Build a correctly formated xml betfair request message to be send with \code{'\link{bfRequest}'}.
##'
##' See \code{betfair:::.bfServers} for list of available servers.
##' Used to prepare the message to \code{'link{bfRequest}'}.
##' @title bfBuildMessage
##' @param operation
##' @param ...   parameter = value pairs
##' @param parameters  named list of parameters
##' @param prefix namespace prefix to be used
##' @param ns main namespace url
##' @param tp types namespace  url
##' @param sessionToken
##' @return \code{'\link[=bfSimpleOutput-class]{bfSimpleOutput}'} object, xml node or S4 object,  as specified by the \code{output} parameter
##' @seealso \code{'\link{betfairly-package}'} \code{'\link{bfRequest}'}
##' @references \url{https://docs.developer.betfair.com/betfair/}
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @keywords internal
bfBuildMessage <- function(operation, parameters = list(), prefix, ns, tp, sessionToken = .sessionToken){
    params <- bfCollapseParams(parameters)
    req_string <- .request_strings[[operation]] #kludge only getAccountStatement uses req
    paste(
          "<soapenv:Envelope xmlns:soapenv='http://schemas.xmlsoap.org/soap/envelope/'
 ", ns, "\n", tp, ">
 <soapenv:Header/>
 <soapenv:Body>
    <", prefix, ":", operation, ">
      <", prefix, ":", req_string, ">
         <header>
           <clientStamp>0</clientStamp>
           <sessionToken>", sessionToken, "</sessionToken >
         </header>\n",
          params,
          "
      </", prefix, ":", req_string, ">
    </", prefix, ":", operation, ">
 </soapenv:Body>
</soapenv:Envelope>", sep = "")
}

.bfRequestInternal <- function(..., operation, curlOpts = list(), server = getOption("bfServer")){
    if(!is.null(server) && !server %in% c("AU", "UK")) stop("server must be 'AU', 'UK', or NULL, suplied ", server)
    server_name <- .bfOperations[[paste(server, operation, sep = "")]]
    if(is.null(server_name)) stop("Unknown operation ", operation, " at ", ifelse(is.null(server), "UK", server), " server")
    server <- .bfServers[[server_name]]
    mess <- bfBuildMessage(operation, parameters = list(...),
                           prefix = server[["prefix"]], ns = server[["ns"]], tp=server[["tp"]])
    bfRequest(mess, operation,  url = server$url,  curlOpts = curlOpts)
}

bfGenericRequest <- function(call){
    call[["operation"]] <- as.character(call[[1]])
    call[[1]] <- as.name(".bfRequestInternal")
    eval(call, parent.frame(1L))  ## always work?
}

## bfGenericRequest1 <- function(call){
##     call[["operation"]] <- as.character(call[[1]])
##     call[[1]] <- as.name(".bfRequestInternal")
##     eval(call, parent.frame(1L))  ## always work?
## }
## environment(bfGenericRequest1) <- getNamespace("betfair")
## assignInNamespace("bfGenericRequest", bfGenericRequest, getNamespace("betfair"))
#### RETURN VALUE:
returnBFOutput <- function(res, output, simplifun = .simplifunDefault, ...){
    ## simpleNode is the node to be analised by simplifun
    if(is.null(output)) output <- "simple"
    if(!is.character(output)) stop("output parameter must be a string")
    switch(output,
           simple=,
           simplify=simplifun(res, ...),
           XML = ,
           xml = res,
           list=fromBFXML(res, forceList = TRUE),
           S4 =,
           s4 = fromBFXML(res),
           stop("Unknown output specification: ", output)
           )
}

.simplifunDefault <- function(res, data_slots = NULL,  info_slots = NULL, exclude_info_slots = NULL
                              , simpleNode = NULL, converters = list(), letMeParseFunc = NULL, classPostfix = ""){
    bfType <- gsub("^[a-zA-Z0-9]+:", "", xmlAttrs(res)[["type"]])
    errorCode <- xmlValue(res[["errorCode"]])
    minorErrorCode <- xmlValue(res[["minorErrorCode"]])
    class <- paste(bfType, "Simple", classPostfix, sep = "")
    if(!(is.na(errorCode) || errorCode == "OK") ||
       !(is.na(minorErrorCode) || minorErrorCode == ""))
        return(new(class, errorCode = errorCode, minorErrorCode = minorErrorCode, bfType = bfType))
    if(!is.null(letMeParseFunc))
        return(do.call(new, c(list(Class = class, errorCode = errorCode, minorErrorCode = minorErrorCode, bfType = bfType), letMeParseFunc(res))))
    if(!is.null(simpleNode))
        res <- res[[simpleNode]]
    res <- fromBFXML(res,  converters = converters,  forceList = TRUE)
    if(length(data_slots)){
        data <- res[data_slots]
        res[data_slots] <- NULL
        isNA <- sapply(data, is1NA)
        data[isNA] <- NULL ## kludge with NA returned by fromXML ..sucks
    }else data <- list()
    if(!is.null(info_slots)) res <- res[info_slots]
    if(!is.null(exclude_info_slots)){
        toecl <- names(res) %in% exclude_info_slots
        res <- res[!toecl]
    }
    do.call(new, c(list(Class = class, res, names = names(res),
                        bfType = bfType, errorCode = errorCode, minorErrorCode = minorErrorCode),
                   data))
}


#### CONVERSIONS:
##' Convert R date/date-time objects into valid XMLSchema representation as
##' required by betfair interface.
##'
##' @title Convert R date-time objects into XMLSchema types as required by betfair interface.
##' @param x Any R date/date-time object or character string which is recognized
##' by as.POSIXlt.
##' @param tz if supplied should be of the form (('+' | '-') hh ':' mm). For
##' example +05:20 means 5 hours 20 minutes before the UTC. And +01:00 is
##' equivalent to CEST.
##' @export
##' @return time string in appropriate format
##' @references http://www.w3.org/TR/xmlschema-2/#dateTime
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
asBFDateTime <- function(x, tz = "Z")
    ## tz should be in format (('+' | '-') hh ':' mm) no checks
    paste(format(as.POSIXlt(x), "%FT%H:%M:%OS3"), tz, sep = "")

## asBFDate <- function(x, tz = "Z"){
##     ## tz should be in format (('+' | '-') hh ':' mm) no checks
##     paste(format(as.POSIXlt(x), "%FT00:00:00"), tz, sep = "")
## }

##     as.POSIXct("1999-12-05 23:02:12")
## format(as.POSIXct("1999-12-05 23:02:12"), "%FT%T%zz")
## format(Sys.time(), "%FT%T")

toBFPOSIX <- function(from) as.POSIXct(as.integer(substr(from, 1, 10)), origin="1970-01-01", tz="GMT")
toBFArray <- function(array, type, maxlength = NULL){
    if(length(array) == 0L) return("")
    if(!is.null(maxlength) && length(array)>maxlength)
        stop("Maximum allowed length of an array of type ", type, " is ", maxlength, ". Actual - ", length(array))
    paste(paste("<tp:", type, ">", array, "</tp:", type, ">", sep = ""), collapse = "\n")
}

asBF <- function(obj, type){
    switch(type ,
           ArrayOfInt= toBFArray(obj,type="int"),
           ArrayOfCountryCode= toBFArray(obj, type = "Country"),
           ArrayOfPlaceBets = toBFArray(obj, type = "PlaceBets", 60),
           ArrayOfCancelBets = toBFArray(paste("<betId>", obj, "</betId>", sep = ""), type = "CancelBets", 40),
           ArrayOfUpdateBets = toBFArray(obj, type = "UpdateBets", 15),
           ArrayOfMarketTypeEnum = toBFArray(obj, type = "MarketTypeEnum"),
           ArrayOfBetId = toBFArray(obj, type = "BetId", 200),
           stop("Unknown type ", type))
}


bfArrayToDataFrame <- function(x, ...){
    if(is.character(x)) return(if(x == "") data.frame() else x)
    x <- .fromBFXML_list(x, forceList=TRUE)
    if(is1NA(x)) return(data.frame())
    as.data.frame(x, stringsAsFactors = FALSE)
}

bfArrayToDataFrame2 <- function(x, ...){
    if(is.character(x)) return(if(x == "") data.frame() else x)
    x <- .fromBFXML_list(x)
    if(length(x) == 0L)
        return(data.frame())
    x <- do.call(rbind, lapply(x, function(y) as.data.frame(y, stringsAsFactors = FALSE)))
    rownames(x) <- NULL
    x
}


#### fromBFXML (S4 CONVERTERS):

.fromBFXML_default <- function(node, obj, type = NULL, converters = list(), forceList = FALSE){
    val <- xmlApply(node, fromBFXML, converters = converters, forceList = forceList)
    if(forceList) return(val)
    for (nm in names(val)) {
        slotName = if (nm %in% ReservedSlotNames)  toupper(nm)  else nm
        if (!is(val[[nm]], class(slot(obj, slotName))))
            if(canCoerce(to_coerce <- val[[nm]], cls <- class(slot(obj, slotName))))
                val[[nm]] <- as(to_coerce, cls)
            else{
                ext <- extends(cls)
                while(length(ext) && !canCoerce(to_coerce, ext[[1]])){
                    ext <- ext[-1]
                }
                if(length(ext) == 0L) stop("Please define setAs methods to coerce ", class(to_coerce), " into ", cls)
                setAs(class(to_coerce), cls, eval(substitute(function(from) new(to_cls2, as(from, to_cls1)),
                                                             list(to_cls1 = ext[[1L]], to_cls2 = cls))))
                warning("Defined new coercion methods from class ", class(to_coerce), " to ", cls)
                val[[nm]] <- as(to_coerce, cls)
            }
        slot(obj, slotName) <- val[[nm]]
    }
    obj
}

.fromBFXML_missing <- function(node, obj = NULL, type = NULL, converters = list(), forceList = FALSE){
    if(is.list(converters)){ ## first entrance
        .converters <- new.env(parent = defaultStructBFConverters)
        if(length(converters))
            for(nm in names(converters))
                .converters[[nm]] <- converters[[nm]]
    }else{
        .converters <- converters
    }
    a <- xmlAttrs(node)
    if (!is.null(a) && missing(type)){ #if supplied, type overwrites the default
        if (!is.na(match("null", names(a))) || !is.na(match("nil", names(a))))
            return(NA) # NULL is difficult to include in data frames,
                                        # use is1NA to check for this value
        if (is.null(type) && !is.na(match("type", names(a))))
            type <- a[["type"]]
    }
    if(isXMLStructure(node)){ ##xmlSize is not reliable
        if (exists(type, .converters))
            get(type, envir = .converters)(node, converters = .converters) # struct Converters have precedence!! and apply to node!
        else if(!is.null(getClassDef(type <- gsub("^[a-zA-Z0-9]+:", "", type))))
            fromBFXML(node, obj = new(type), converters = .converters, forceList = forceList) #  inheritance over obj
        else
            .fromBFXML_list(node, converters = .converters, forceList = forceList) # treat as leas if unknown class
    }else{  ## primitive type
        func <- defaultSimpleBFConverters[[type]]
        if (is.null(func)){
            warning("Don't understand the SOAP type `", type, "' yet")
            xmlValue(node)
        }else{
            ## print(node)
            func(xmlValue(node))
        }
    }
}

.fromBFXML_list <- function(node, obj, type = NULL, converters = list(), forceList = TRUE)
    xmlApply(node, fromBFXML, converters = converters,  forceList = forceList)
.fromBFXML_vector <-  function(node, obj, type = NULL, converters = list(), forceList = TRUE)
    unlist(xmlApply(node, fromBFXML, converters = converters, forceList = forceList))

##' Methods for function \code{fromBFXML}.
##'
##' The function \code{fromBFXML} is used internally to convert xml nodes
##' resulted from betfair API requests into valid S4 objects as specified by
##' betfair service WSDL description.  Methods are dispatched only for the
##' second argument \code{obj}.
##'
##'
##' S4 equivalents for betfair types are not provided with the
##' package. You need to initialize them with
##'
##' \code{   bfInitClasses()}
##'
##' You need \code{XMLSchema} package for this.
##'
##' Once installed,  you can use \code{str(getClass(foo))} to view the strucutre
##' of class \code{'foo'}.
##'
##' }
##' \section{\strong{Methods}}{ \describe{
##'
##'     \item{\code{signature(obj = "ANY")}}{ \code{obj} should be a
##' valid S4 object with the same structure as node. It is usually
##' created with \code{new(objClass)},  where \code{objClass} is the
##' class generated with the \code{defineClasses} function from
##' \code{XMLSchema} package, or \code{'SSOAP'} package function
##' \code{\link[SSOAP]{genSOAPClientInterface}}.
##'
##' The function ties hard to find and install all the \code{"as"}
##' methods. In rare cases it will fail. To make it work you have to
##' define the specific conversion yourself.
##'
##' \code{'type'} argument is ignored in this method.
##'
##'     }
##'
##'     \item{\code{signature(obj = "list")}}{
##'         All elements of the \code{node} are converted in corresponding elements of a list.
##'         \code{'type'} argument is ignored.
##'     }
##'
##'     \item{\code{signature(obj = "missing")}}{
##'        Target S4 class is taken from \code{'type'} argument,  or if missing is inferred from the node itself.
##'     }
##'
##'     \item{\code{signature(obj = "vector")}}{
##'        Like the \code{list} method but unlist it's argument,  to make it a vector.
##'     }
##' }
##'
##' @title Convert an XML object into S4 object.
##' @param node XML node (as one returned by \code{bfRequest})
##' @param obj See details
##' @param type betfair type (equivalently S4 class name) of the output
##' @param converters a list of functions to be used to convert undefined classes (see the object \code{defaultStructBFConverters})
##' @param forceList TRUE for recursive list mirroring the node
##' @seealso \code{'\link{betfairly-package}'} \code{'\link{bfInitClasses}'}
##' @references https://docs.developer.betfair.com/betfair/
##' @author Vitalie Spinu
##' @keywords internal methods
setGeneric("fromBFXML",
           function(node,  obj,  type = NULL, converters = list(), forceList = FALSE)
           standardGeneric("fromBFXML"), signature = c("obj"),
           useAsDefault = .fromBFXML_default)
eval({
    ## prohibit roxygen from creating spurious docs
    setMethod("fromBFXML",  "missing", .fromBFXML_missing)
    setMethod("fromBFXML",  "vector", .fromBFXML_vector)
    setMethod("fromBFXML",  "list", .fromBFXML_list)
})


#### DEFAULT FUNCTIONS TO USE AS simpleOutput:

.runners_types <- c(runner = "integer", selectionId = "integer", sortOrder = "integer", totalAmount = "numeric",
                    lastPrice = "numeric", handicap = "numeric", reduction = "numeric",
                    vacant = "logical", farBSP = "numeric", nearBSP = "numeric", actualBSP = "numeric")
.prices_types <- c(runner = "integer", bestPrice = "character", price = "numeric", type = "character", amount = "numeric", depth = "integer")
.priceInfo_types <- c(marketId = "integer", currencyCode = "character", marketStatus = "character", ##"MarketStatusEnum",
                      delay = "integer", numberOfWinners = "integer", marketInfo = "character",
                      discountAllowed = "logical", marketBaseRate = "character", lastRefresh = "character",
                      removedRunners = "character", bspMarket = "logical")

.simple_getMarketPricesCompressed <- function(res, ...){
    res <- xmlValue(res[["marketPrices"]])
    if(!nzchar(res)) return(NULL)
    res <- bfSplit(res, ":")
    priceInfo <- as.list(bfSplit(res[[1L]]))
    if(nzchar(priceInfo[[10L]])){ ## removed runners are here
        t1 <- bfSplit(priceInfo[[10L]], ";")
        priceInfo[[10L]] <- t1[-length(t1)]
        priceInfo[[11L]] <- t1[[length(t1)]]
    }
    names(priceInfo) <- names(.priceInfo_types)
    for(nm in names(priceInfo))
        priceInfo[[nm]] <- as(priceInfo[[nm]], .priceInfo_types[[nm]])
    res <- res[-1L]
    runners <- toLay <- toBack <- list()
    for(runner in seq_along(res)){
        R <- unlist(strsplit(res[[runner]], "|", fixed = TRUE))
        runners[[runner]] <- c(runner, strsplit(R[[1L]], "~", fixed = TRUE)[[1]])
        if(nzchar(R[[2L]])){
            m <- matrix(strsplit(R[[2L]], "~", fixed = TRUE)[[1L]], ncol = 4L, byrow = T,
                        dimnames = list(NULL, c("price", "amount","type", "depth")))
            toBack[[runner]] <- cbind(runner, bestPrice = "toBack", m)
        }
        if(length(R) == 3L && nzchar(R[[3L]])){
            m <- matrix(strsplit(R[[3L]], "~", fixed = TRUE)[[1L]], ncol = 4L, byrow = T,
                        dimnames = list(NULL, c("price", "amount", "type", "depth")))
            toLay[[runner]] <- cbind(runner, bestPrice = "toLay", m)
        }
    }
    if(length(runners)){
        runners <- as.data.frame(do.call(rbind, runners), stringsAsFactors = FALSE)
        names(runners) <- names(.runners_types)
        for(nm in names(runners)[.runners_types != "character"])
            runners[[nm]] <- as(runners[[nm]], .runners_types[[nm]])
    }else{
        runners <- NULL
    }
    if(length(toBack) || length(toLay)){
        prices <- as.data.frame(do.call(rbind, c(toBack, toLay)), stringsAsFactors = FALSE)
        for(nm in names(prices)[.prices_types != "character"])
            prices[[nm]] <- as(prices[[nm]], .prices_types[[nm]])
        prices <- prices[-5L]  ## type is not needed
    }else{
        prices <- NULL
    }
    list( runners = runners,
        prices = prices)
}

## getMarketPricesCompressed(nm)
## tex <- "5082333~GBP~ACTIVE~0~1~NR\\: (RSA) <br>8. FanMail (0%,11\\:07), 6(2.5%,11\\:08)~true~5.0~1162835723938~6. Earlswood,9.08,2.5;8.Fan Mail,9.07,2.4;Y:1058616~0~6.04~8.4~~11.9~false||:670160~1~6.18~17.5~~4.2~false||:1132008~2~9.78~5.2~~20.4~false||:894820~3~140.02~4.6~~20.4~false||1.01~5.0~B~1~:1414415~4~8.2~6.2~~16.0~false||:575636~5~5.54~11.5~~8.6~false||:1433455~6~0.0~~~0.4~false||:1433456~7~0.0~~~0.9~false||:746753~8~5.54~11.5~~5.2~false||:1433457~9~0.0~~~4.2~false||:1147548~10~0.0~~~2.6~false||:1433458~11~62.46~2.0~~3.5~false||:1433459~12~0.0~~~0.9~false||:1433460~13~0.0~~~0.9~false||"
## .simple_getMarketPricesCompressed(tex) ## wrong example on the site

.simple_getMarketTradedVolumeCompressed <- function(res, ...){
    res <- xmlValue(res[["tradedVolume"]])
    if(!nzchar(res)) return(NULL)
    res <- strsplit(res, ":", fixed = TRUE)[[1]]
    if(!nzchar(res[[1]])) res <- res[-1]
    runners <- list()
    volumes <- list()
    for(i in seq_along(res)){
        sl <- strsplit(res[[i]], "|", fixed = TRUE)[[1]]
        runners[[i]] <- unlist(strsplit(sl[[1]], "~", fixed = TRUE), use.names = FALSE)
        if(length(sl <- sl[-1])) # otherwise null
            volumes[[i]] <- cbind(runner = i, matrix(unlist(strsplit(unlist(sl), "~", fixed = TRUE)), ncol = 2, byrow = T, dimnames = list(NULL, c("Odds", "totalMatchedAmount"))))
    }

    runners <- data.frame(seq_along(res), do.call(rbind, runners), stringsAsFactors = F)
    names(runners) <- c("runner", "selectionId", "asianLineId", "actualBSP", "totalBspBackMatchedAmount", "totalBspLiabilityMatchedAmount")
    for(nm in c("runner", "asianLineId", "selectionId"))
        runners[[nm]] <- as.integer(runners[[nm]])
    for(nm in c("actualBSP", "totalBspBackMatchedAmount", "totalBspLiabilityMatchedAmount"))
        runners[[nm]] <- as.double(runners[[nm]])

    volumes <- do.call(rbind, volumes)
    storage.mode(volumes) <- "double"
    volumes <- as.data.frame(volumes, stringsAsFactors = FALSE)
    volumes[["runner"]] <- as.integer(volumes[["runner"]])
    list(runners = runners, volumes = volumes)
}

.simple_GetBetHistory <- function(res, ...){
    res <- res[["betHistoryItems"]]
    res <- fromBFXML(res, forceList = TRUE)
    errorCode <- xmlValue(res[["errorCode"]])
    minorErrorCode <- xmlValue(res[["minorErrorCode"]])
    if(is1NA(res)) return( new("GetBetHistoryRespSimple", errorCode = errorCode, minorErrorCode = minorErrorCode))
    len <- length(res)
    matches <- list()
    for(i in 1:len){
        matches[res[[i]][["betId"]]] <- res[[i]][["matches"]]
        res[[i]][["matches"]] <- NULL
    }
    res <- do.call(rbind, lapply(res, function(y) as.data.frame(y, stringsAsFactors = FALSE)))
    rownames(res) <- res$betId
    dNames <- grep("Date", names(res))
    res <- res[, c((1:length(res))[-dNames], dNames)]
    res$betId <- NULL
    if(is1NA(matches[[1]])){
        matches <- data.frame()
    }else{
        matches <- do.call(rbind, lapply(matches, function(y) as.data.frame(y, stringsAsFactors = FALSE)))
        dNames <- grep("Date", names(matches), fixed = TRUE)
        matches <- matches[, c((1:length(matches))[-dNames], dNames)]
    }
    list(matches = matches, betHistoryItems = res)
}


### AUXILIARY S4 SUPPORT

bfDefClass <-  function(i, where = globalenv(), namespaceDefs = list(), verbose = FALSE,
                        pending = NULL, classes = new.env(hash = TRUE, emptyenv()),
                        types = NULL, baseClass = "VirtualSOAPClass", force = FALSE,
                        name = if (is(i, "GenericSchemaType")) i@name else i$name)
{
    ## slightly adapted version of defClass from XMLSchema package (the original one does not install the classes of an element)
    if (is.null(i))
        return(FALSE)
    if (is(i, "SchemaTypes")) {
        return(lapply(i, bfDefClass, where, namespaceDefs, verbose,
                      pending, classes, types, baseClass, force))
    }
    if (verbose) {
        cat("<bfDefClass>", name, "\n")
        on.exit(cat("finished", name, "\n"))
    }
    ## if (name == "ResourceIdSetType") {
    ##     unlockBinding("showDefClassTrace", getNamespace("XMLSchema"))
    ##     showDefClassTrace <<- TRUE
    ## }
    ## if (showDefClassTrace)
    ##     print(sys.calls())
    if (is(i, "XMLAbstractNode") || is.null(i)) {
        return(NA)
    }
    type = i
    if (is(type, "SchemaTypes")) {
        return(lapply(type, bfDefClass, where, namespaceDefs, verbose,
                      pending, classes, types, baseClass, force))
    }
    if (!force && !is.null(getClassDef(name, where)))
        return(structure(FALSE, names = "class already exists"))
    if (exists(name, pending))
        return(structure(NA, names = "pending definition"))
    assign(name, "TRUE", pending)
    on.exit({
        if(verbose && exists(name, pending)){ ##vs aded the exist statement
            cat("Removing ", name, "from pending\n")
            remove(list = name, envir = pending)
        }
    })
    if (is(i, "AnySOAPType")) {
        if (verbose)
            cat("defining", name, "\n")
        setClass(name, where = where)
        return(TRUE)
    }
    o = i
    if (!is(i, "GenericSchemaType"))
        i = i$definition
    if (is(i, "RestrictedStringDefinition")) {
        valid = function(object) {
            values = ""
            if (!any(object == values))
                stop(object, " is not a recognized value ", paste(sQuote(values),
                                                                  collapse = ", "))
            TRUE
        }
        body(valid)[[2]][[3]] = i@values
        def = setClass(name, contains = "character", validity = valid,
          where = where)
    }
    else if (is(i, "RestrictedSetInteger")) {
        def = setClass(name, "integer", where = where)
        fun = function(from) XMLSchema:::asIntegerSetValue(from, "a", "b")
        body(fun)[[3]] = i@values
        body(fun)[[4]] = name
        setAs("numeric", name, fun, where = where)
    }
    else if (is(i, "EnumValuesDef")) {
        elName = paste(name, "Values", sep = "_")
        assign(elName, as.character(i@values), envir = where)
        f = NULL
        if (verbose)
            cat("defining class", name, "\n")
        def = setClass(name, contains = c("character", baseClass),
          validity = f, where = where)
    }
    else if (is(i, "ClassDefinition")) {
        def = XMLSchema:::defineClassDefinition(i, types, namespaceDefs,
          name, classes, pending, baseClass, where, verbose,
          force)
    }
    else if (is(i, "Element")) {
        if (verbose)
            cat("<bfDefClass>element", type@name, "\n")
        remove(list = name, envir = pending) ## vs added
        def = bfDefClass(i@type, where, namespaceDefs, verbose,
          pending, classes, types, baseClass, force, name = type@name)
        return(def)
    }
    else if (is(i, "SimpleSequenceType")) {
        def = XMLSchema:::createArrayClass(i, types, where = where, verbose = verbose)
    }
    else if (is(i, "SOAPComplexType")) {
        if (verbose)
            cat("defining", i@name, " (temporary solution)\n")
        setClass(i@name, where = where)
        return()
    }
    else if (is(i, "UnionDefinition")) {
        XMLSchema:::defUnionClass(i, types, name = name, where, verbose = verbose,
                      force = force)
    }
    else {
        warning("bfDefClass: no code to handle ", class(i), " for ",
                i@name)
        def = NULL
    }
    if (!is.null(def)) {
        if (is(type, "BasicSOAPType") && length(body(type@fromConverter)) >
            1) {
            if (verbose)
                cat("defining setAs() for", type@name, "\n")
            if (is(type@fromConverter, "SOAPElementConverter"))
                cvt = as(type@fromConverter, "AsFunction")
            else cvt = type@fromConverter
            setAs("XMLAbstractNode", type@name, cvt, where = where)
        }
        assign(name, def, classes)
        def
    }
    else NULL
}

bfGenerateClasses <- function(types, verbose = FALSE, where = .GlobalEnv){
    if(!verbose)
        cat("\nDefining betfair classes from name spaces:\n",
            paste(names(types), collapse = "\n"), "\n...\n", sep = "")
    pending = new.env(hash = TRUE, emptyenv())
    classes = new.env(hash = TRUE, emptyenv())
    out <- lapply(types, function(schema) lapply(schema, bfDefClass, where = where,
                                                 namespaceDefs = list(), verbose = verbose,
                                                 pending = pending, classes = classes,
                                                 types = types, baseClass = "betfair",
                                                 force = TRUE))
    cat("Defined ", sum(sapply(out, length)), " classes.\n")
}

##' All betfair functions are capable of producing a valid S4 object
##' corresponding to betfair SOAP specification. \cr For parsimony reasons the
##' betfair S4 classes are not installed with the package. You need \code{
##' bfInitClasses()} to initialize them. Note that \code{XMLSchema} package is
##' required for this initialization.
##'
##' @title Initialize betfair S4 interface
##' @param verbose  Print info message for each class
##' @param where Environment in which to store the class definitions; defaults to global environment.
##' @return \code{'\link[=bfSimpleOutput-class]{bfSimpleOutput}'} object, xml node or S4 object,  as specified by the \code{output} parameter
##' @seealso \code{'\link{betfairly-package}'} \code{'\link{bfSimpleOutput-class}'}
##' @references \url{https://docs.developer.betfair.com/betfair/}
##' @author Vitalie Spinu (\email{spinuvit@@gmail.com})
##' @export
##' @examples
##' \dontrun{
##' install.packages("XMLSchema", repos = "http://www.omegahat.org/R")  ## windows binaries?
##' install.packages("XMLSchema", repos = "http://www.omegahat.org/R", type = "source")  ## from source
##' bfInitClasses()
##' }
##'
bfInitClasses <- function(verbose = FALSE, where = .GlobalEnv){
    if(!require(XMLSchema))
        stop("You need to install the XMLSchema package from omegahat.\n Try install.packages('XMLSchema', repos = 'http://www.omegahat.org/R', type = 'source')")
    load(paste(.path.package("betfairly"), "/extdata/types_data.rda", sep = ""))
    ## write(getURI("https://api.betfair.com/global/v3/BFGlobalService.wsdl"), tmp <- tempfile())
    ## Gl <- processWSDL(tmp)
    ## cat("Processing AU BFExchangeService.wsdl\n")
    ## write(getURI("https://api-au.betfair.com/exchange/v5/BFExchangeService.wsdl"), tmp <- tempfile())
    ## ExAU <- processWSDL(tmp)
    ## cat("Processing BFExchangeService.wsdl\n")
    ## write(getURI("https://api.betfair.com/exchange/v5/BFExchangeService.wsdl "),tmp <- tempfile())
    ## ExUK <- processWSDL(tmp)
### CLASSES
    bfGenerateClasses(Gl_types, verbose = verbose)
    bfGenerateClasses(ExUK_types, verbose = verbose)
}



## Local Variables:
## ess-roxy-template-alist: (
##  ("description" . "..description")
##  ("details" . "..details")
##  ("title" . "")
##  ("param" . "")
##  ("return" . "\\code{'\\link[=bfSimpleOutput-class]{bfSimpleOutput}'} object, xml node or S4 object,  as specified by the \\code{output} parameter")
##  ("seealso" . "\\code{'\\link{betfairly-package}'} \\code{'\\link{bfSimpleOutput-class}'}")
##  ("references" . "\\url{https://docs.developer.betfair.com/betfair/}")
##  ("author" . "Vitalie Spinu (\\email{spinuvit@@gmail.com})")
## )
## end:

