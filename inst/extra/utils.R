## in inst directory

## package.skeleton("betfair1", enviro = new.env(),
##                  force = TRUE, namespace = TRUE, code_files = c("betfair.R", "funcs.R"))
## library(roxygen)

setwd("../..") ## parent of betfairly dir
getwd()
.build_sysdata()
## system("cp types_data.rda betfairly/inst/extdata/")
library(roxygen)

options(width = 80)
system("rm -r betfairly.roxygen")
roxygenize("betfairly", "betfairly.roxygen", use.Rd2 = T)
## roxygen2:::roxygenize("betfairly", "betfairly.roxygen")
## system("rm betfairly.roxygen.pdf")
## system("R CMD Rd2pdf betfairly.roxygen")
system("cp sysdata.rda betfairly.roxygen/R/")
## system("rm betfairly.roxygen/R/sysdata.rda")
## system("R CMD check betfairly.roxygen")
## tools::showNonASCII(readLines("./betfairly/R/betfair.R"))

system("rm betfairly.roxygen/R/.Rhistory")
system("R CMD build betfairly.roxygen")

system("R CMD check betfairly_1.0.tar.gz")

install.packages("betfairly_1.0.tar.gz")
sdf



### PACKAGE BUILD UTILITY
.build_sysdata <- function(file = "sysdata.rda", typesfile = "types_data.rda"){
    ## used to prepare sysdata.rda,  an utility function
    ## library(SSOAP)
    library(RCurl)
    library(XML)
    library(XMLSchema)
    library(SSOAP)
    .makeBFServer <- function(sdesc){
        ##make name-spaces to be used in messages; sdesk is a server Description as returned by processWSDL in SSOAP package
        name <- sdesc@name
        prefix <- tolower(substring(name, 1, 4))
        url <- SSOAP:::toURL(sdesc@server)
        types_urls <- names(sdesc@types)
        is_type <- grepl("types", types_urls, fixed = T)
        stopifnot(sum(is_type) == 1L && sum(!is_type) == 1L)
        main_nspace <- types_urls[!is_type]
        types_nspace <- types_urls[is_type]
        ns <- sprintf("xmlns:%s='%s'", prefix, main_nspace)
        tp <- sprintf("xmlns:tp='%s'", types_nspace )
        list(name = name, url = url,
             main_nspace = main_nspace,
             types_nspace = types_nspace,
             prefix = prefix, ns = ns, tp = tp)
    }
###  WSDL
    ## write(getURI("https://api.betfair.com/global/v3/BFGlobalService.wsdl"), "BFGlobalService.wsdl")
    ## write(getURI("https://api-au.betfair.com/exchange/v5/BFExchangeService.wsdl"), "BFExchangeServiceAU.wsdl")
    ## write(getURI("https://api.betfair.com/exchange/v5/BFExchangeService.wsdl "), "BFExchangeService.wsdl")
    Gl <- processWSDL("BFGlobalService.wsdl")
    ExUK <- processWSDL("BFExchangeService.wsdl")
    ExAU <- processWSDL("BFExchangeServiceAU.wsdl")
    ## glcls <- new.env(parent = globalenv())
    ## glenv <- new.env(parent = glcls)
    ## ## gl <- genSOAPClientInterface(, Gl, verbose = F, env = glenv, where = glcls, force = T)
    ## excls <- new.env(parent = globalenv())
    ## exenv <- new.env(parent = excls)
    ## ex <- genSOAPClientInterface(, ExUK, verbose = F, env = exenv, where = excls, force = T)
### CLASSES
    ## Gl_env <- new.env()
    ## ExUK_env <- new.env()
    ## Gl_classes <- defineClasses(Gl@types, where = Gl_env, verbose = TRUE, force = TRUE)
    ## ExUK_classes <- defineClasses(ExUK@types, where = ExUK_env, verbose = TRUE, force = TRUE)
    Gl_types <- Gl@types
    ExUK_types <- ExUK@types
    ExAU_types <- ExAU@types
### .bfServers
    .bfServers <- list()
    .bfOperations <- new.env(hash = TRUE, parent = emptyenv())
    ExUK@name <- paste("UK", ExUK@name, sep = "")
    ExAU@name <- paste("AU", ExAU@name, sep = "")
    .bfServers[c(Gl@name, ExUK@name, ExAU@name)]  <- list(.makeBFServer(Gl), .makeBFServer(ExUK), .makeBFServer(ExAU))
    ## unprefixed
    invisible(lapply(Gl@operations, function(el)
                     lapply(names(el), assign, Gl@name, envir = .bfOperations)))
    invisible(lapply(ExUK@operations, function(el)
                     lapply(names(el),  assign, ExUK@name, envir = .bfOperations))) ## uk functions withoug prefix
    ## UK prefix
    invisible(lapply(Gl@operations, function(el)
                     lapply(paste("UK", names(el), sep = ""), assign, Gl@name, envir = .bfOperations)))
    invisible(lapply(ExUK@operations, function(el)
                     lapply(paste("UK", names(el), sep = ""),  assign, ExUK@name, envir = .bfOperations))) ## uk with prefix
    ## AU prefix
    invisible(lapply(Gl@operations, function(el)
                     lapply(paste("AU", names(el), sep = ""), assign, Gl@name, envir = .bfOperations)))
    invisible(lapply(ExAU@operations, function(el)
                     lapply(paste("AU", names(el), sep = ""), assign, ExAU@name, envir = .bfOperations)))
### DEFAULT CONVERTERS
    ReservedSlotNames <- XMLSchema:::ReservedSlotNames
    defaultSimpleBFConverters <- list2env(XMLSchema:::SchemaPrimitiveConverters, hash = T, parent = emptyenv())
    defaultStructBFConverters <- new.env(parent = emptyenv())
    allBFTypes <- unlist(c(lapply(Gl@types, names), lapply(ExUK@types, names)), use.names= F)
    character_converters <- c("xsd:long",  "xsd:dateTime", paste("n2:", grep("Enum$", allBFTypes, value = T), sep = ""))
    ## "n2:MarketStatusEnum",
    ## "n2:MarketTypeEnum", "n2:MarketTypeVariantEnum", "n2:MarketTypeVariantEnum",
    ## "n2:BetTypeEnum", "n2:GetEventsErrorEnum", "n2:APIErrorEnum")
    identity <- function(x) x
    for(nm in character_converters)
        defaultSimpleBFConverters[[nm]] <- identity
    na_converters <- paste("n2:", grep("^Array", allBFTypes, value = T), sep = "")
    ## c("n2:ArrayOfRunnerPrices", "n2:ArrayOfEventId", "n2:ArrayOfPrice", "n2:ArrayOfRunner",
    ##   "n2:ArrayOfMarketSummary", "n2:ArrayOfCouponLinks")
    for(nm in na_converters)
        defaultSimpleBFConverters[[nm]] <- function(x, ...) if(nzchar(x)) x else NA
    for(nm in ls(defaultSimpleBFConverters, all.names = TRUE))
        environment(defaultSimpleBFConverters[[nm]]) <- baseenv()
### request strings KLUDGE: getAccountStatement uses req instead of request.
    .request_strings <- new.env(parent = emptyenv())
    tnames <- names(Gl@types[[2]])
    tnames <- tnames[-grep("Response$", tnames)]
    for(nm in tnames)
        .request_strings[[nm]] <- names(Gl@types[[2]][[nm]]@type@slotTypes)
    tnames <- names(ExUK@types[[2]])
    tnames <- tnames[-grep("Response$", tnames)]
    for(nm in tnames)
        .request_strings[[nm]] <- names(ExUK@types[[2]][[nm]]@type@slotTypes)
### SAVE
    save(.bfServers,
         .bfOperations,
         ReservedSlotNames, .request_strings,
         defaultSimpleBFConverters,
         defaultStructBFConverters,
         file = file, compress = "bzip2")
    save(Gl_types, ExUK_types, ExAU_types,
         file = typesfile, compress = "bzip2")
}


## ### my data colectors
## getUsefulMarkets <- function(limit = 100){
##     evs <- getActiveEventTypes()
##     mts <- getAllMarkets(1)
##     mts <- subset(mts, marketName == "Match Odds" & totalMatched > limit)
##     inames <- c("marketID",  "countryCode","totalMatched", "marketType", "marketStatus",
##                 "eventDate",  "exchangeID", "turningInPlay", "menuPath")
##     snames <- gsub("market", "m", inames)
##     mts <- mts[inames]
##     mts <- mts[order(mts$totalMatched, decreasing=T), ]
##     names(mts) <- snames
##     mts
## }

## getUsefulMarketPrices <- function(mid, time = Sys.time()){
##     m <- getMarketPricesCompressed(mid, "EUR")
##     if(is.null(m$prices)) return(NULL) ## closed or susspended(sometimes) markets
##     cbind(m$prices, mStatus = m@info$marketStatus, delay = m@info$delay, time = Sys.time())
## }

## ## getUsefulMarketPrices(mid)
## .bfLog <- function(txt = "", e, file = "log/bfLog.org", type = "Error"){
##     if(inherits(e, "bfError")){
##         if(e$bfAPIError == "NO_SESSION") bfLogin("vitoshka", "vitamit178")
##     }
##     cat("* [", as.character(Sys.time()), "]--------------- \n", txt,
##         type, e$message, file = file, fill = T, append = TRUE)
##     cat("\n", type, "occurred:", e$message, fill = T)
##     if(type == "Error"){
##         if(!exists("..nr_errors", .GlobalEnv)) assign("..nr_errors", 0L, .GlobalEnv)
##         if(..nr_errors > 50) stop("More than 50 errors occured!!,  execution stoped.")
##         else ..nr_errors <<- ..nr_errors + 1L
##     }else if(type == "Warning"){
##         if(!exists("..nr_warnings", .GlobalEnv)) assign("..nr_warnings", 0L, .GlobalEnv)
##         if(..nr_warnings > 100) stop("More than 100 warnings occured!!,  execution stoped.")
##         else ..nr_warnings <<- ..nr_warnings + 1L
##     }
## }

## bfDataCollector <- function(file_active = "BF_active.RData", file_closed = "BF_closed.RData", sec = 2L, reinit = FALSE){
##     options(warn = 1L)
##     if(reinit) file.remove(file_active, file_closed)
##     if(file.exists(file_active) && file.exists(file_closed)) {
##         load(file_active)
##         load(file_closed)
##         new_mts <- getUsefulMarkets()##[1:3, ]
##         old_id <- new_id
##     }else{
##         new_mts <- getUsefulMarkets()##[1:3, ]
##         old_id <- new_id <- c()
##         mts_all <- NULL
##         mActive <- new.env(parent = .GlobalEnv)
##         mClosed <- new.env(parent = .GlobalEnv)
##     }
##     .local <- function(first = FALSE){
##         new_id  <<- as.character(new_mts$mID)
##         closed <- setdiff(old_id, new_id)
##         new <- setdiff(new_id, old_id)
##         cat("\n[", as.character(Sys.time()), "]\n", sep = "")
##         if(length(new)){
##             which_new <- !new_id %in% old_id
##             cat("+", length(new), "new markets:\n")
##             print(new_mts[which_new, "menuPath"])
##             mts_all  <<- rbind(mts_all, cbind(new_mts[which_new, ], new = !first))
##             lapply(new, assign, NULL, envir = mActive)
##         }
##         if(length(closed)){
##             cat("-", length(closed), "market(s) were closed.\n")
##             lapply(closed, function(nm) assign(nm, mActive[[nm]], envir = mClosed))
##         }
##         remove(list = closed, envir = mActive)
##         cat("Saving data ... ")
##         save(mts_all, mClosed, file = file_closed)
##         save(new_id, mActive, file = file_active)
##         cat("done\n")
##         iter <- 10L
##         cat(sprintf("Updating %d markets %d times, with %d seconds sleep:\n", length(mActive), iter, sec))
##         pb <- txtProgressBar(max = iter, char = "=", width = 50, style = 1L)
##         setTxtProgressBar(pb, 0L)
##         time <- system.time({
##             for(i in 1:iter){
##                 for(nm in ls(mActive)){
##                     tryCatch(mActive[[nm]] <- rbind(mActive[[nm]], ump <- getUsefulMarketPrices(nm)),
##                              error = function(e) .bfLog(nm, e),
##                              warning = function(e) .bfLog(nm, e, type = "Warning"))
##                     Sys.sleep(sec)
##                 }
##                 setTxtProgressBar(pb, i)
##             }
##             close(pb)
##         })
##         cat("Time elapsed:\n")
##         print(time)
##         old_id  <<- new_id
##         new_mts <<- try(getUsefulMarkets())
##     }
##     .local(TRUE)
##     while(TRUE) .local()
## }


## tf <- function(){
##     aa <<- 6
## }


## exists(file_active) && file.exists(file_closed)) {
## load(file_active)
## load(file_closed)
## new_mts <- getUsefulMarkets()##[1:3, ]
## old_id <- new_id
## }else{
## new_mts <- getUsefulMarkets()##[1:3, ]
## old_id <- new_id <- c()
## mts_all <- NULL
## mActive <- new.env(parent = .GlobalEnv)
## mClosed <- new.env(parent = .GlobalEnv)
## }
## .local <- function(first = FALSE){
## new_id  <<- as.character(new_mts$mID)
## closed <- setdiff(old_id, new_id)
## new <- setdiff(new_id, old_id)
## cat("\n[", as.character(Sys.time()), "]\n", sep = "")
## if(length(new)){
##     which_new <- !new_id %in% old_id
##     cat("+", length(new), "new markets:\n")
##     print(new_mts[which_new, "menuPath"])
##     mts_all  <<- rbind(mts_all, cbind(new_mts[which_new, ], new = !first))
##     lapply(new, assign, NULL, envir = mActive)
## }
## if(length(closed)){
##     cat("-", length(closed), "market(s) were closed.\n")
##     lapply(closed, function(nm) assign(nm, mActive[[nm]], envir = mClosed))
## }
## remove(list = closed, envir = mActive)
## cat("Saving data ... ")
## save(mts_all, mClosed, file = file_closed)
## save(new_id, mActive, file = file_active)
## cat("done\n")
## iter <- 10L
## cat(sprintf("Updating %d markets %d times, with %d seconds sleep:\n", length(mActive), iter, sec))
## pb <- txtProgressBar(max = iter, char = "=", width = 50, style = 1L)
## setTxtProgressBar(pb, 0L)
## time <- system.time({
##     for(i in 1:iter){
##         for(nm in ls(mActive)){
##             tryCatch(mActive[[nm]] <- rbind(mActive[[nm]], ump <- getUsefulMarketPrices(nm)),
##                      error = function(e) .bfLog(nm, e),
##                      warning = function(e) .bfLog(nm, e, type = "Warning"))
##             Sys.sleep(sec)
##         }
##         setTxtProgressBar(pb, i)
##     }
##     close(pb)
## })
## cat("Time elapsed:\n")
## print(time)
## old_id  <<- new_id
## new_mts <<- try(getUsefulMarkets())
## }
## .local(TRUE)
## while(TRUE) .local()
## }


## tf <- function(){
##     aa <<- 6
## }
