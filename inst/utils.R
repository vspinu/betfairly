## in inst directory
setwd("betfairly/inst")
.build_sysdata()

## package.skeleton("betfair1", enviro = new.env(),
##                  force = TRUE, namespace = TRUE, code_files = c("betfair.R", "funcs.R"))
## library(roxygen)

setwd("../..") ## parent of betfairly dir
getwd()

options(width = 80)
system("rm -r betfairly.roxygen")
roxygen:::roxygenize("betfairly", "betfairly.roxygen", use.Rd2 = T)
## roxygen2:::roxygenize("betfairly", "betfairly.roxygen")
system("rm betfairly.roxygen.pdf")
system("R CMD Rd2pdf betfairly.roxygen")

system("cp betfairly/inst/sysdata.rda betfairly.roxygen/R/")

system("R CMD check betfairly.roxygen")
tools::showNonASCII(readLines("./betfairly/R/betfair.R"))
system("rm betfairly.roxygen/R/.Rhistory")

system("R CMD build betfairly.roxygen")
install.packages("betfairly_1.0.tar.gz")
sdf

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
