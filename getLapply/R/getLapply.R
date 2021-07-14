#'getLapply-package
#'@aliases NULL
#'@description
#'The getLapply package.
"_PACKAGE"

getLapplyEnv <- new.env()

#'getLapply
#'@description
#'Get a lapply
#'@export
getLapply <- function() {
    get("lapply", envir = getLapplyEnv)
}

#'setLapply
#'@description
#'Set the served lapply.
#'@param x a 'function'
#'@export
setLapply <- function(x = lapply) {
    stopifnot(is.function(x))
    assign("lapply", x, envir = getLapplyEnv)
}

#'getSeededLapply
#'@description
#'Get a lapply
#'@export
getSeededLapply <- function() {
    get("seededLapply", envir = getLapplyEnv)
}

#'setSeededLapply
#'@description
#'Set the served lapply.
#'@param x a 'function' 
#'@export
setSeededLapply <- function(x = lapply) {
    stopifnot(is.function(x))
    assign("seededLapply", x, envir = getLapplyEnv)
}

#'getChunkSize
#'@description
#'Get a chunkSize
#'@export
getChunkSize <- function() {
    get("chunkSize", envir = getLapplyEnv)
}

#'setChunkSize
#'@description
#'Set the served chunkSize
#'@param x an 'integer' defining a sensible max chunk size.
#' Default : \code{.Machine$integer.max}
#'@export
setChunkSize <- function(x = 2^24) {
    stopifnot(length(x) == 1)
    x <- as.integer(x)
    stopifnot(is.integer(x))
    stopifnot(!is.na(x))
    stopifnot(x > 0)
    assign("chunkSize", x, envir = getLapplyEnv)
}


#'getSensibleThreads
#'@export
getSensibleThreads <- function() {
    get("sensibleThreads", envir = getLapplyEnv)
}

#'setSensibleThreads
#'@param x a positive 'integer' defining a sensible number of threads to run. 
#'  Default : \code{max(1,parallel::detectCores()-1)}
#'@importfrom parallel detectCores
#'@export
setSensibleThreads <- function(x = max(1, parallel::detectCores() - 1)) {
    stopifnot(length(x) == 1)
    x <- as.integer(x)
    stopifnot(is.integer(x))
    stopifnot(!is.na(x))
    stopifnot(x > 0)
    assign("sensibleThreads", x, envir = getLapplyEnv)

}

#'chunk
#'@param len the index range to divide
#'@param threads the number of chunks to create
#'@return a list of c(start,end) vectors defining chunks
#'@export
chunk <- function(len, threads = getSensibleThreads()) {
    stopifnot(length(len) == 1 && len%%1 != 0)
    stopifnot(length(threads) == 1 && threads%%1 != 0)
    chunkSize <- len%/%threads

    if (chunkSize < 1 || threads < 2) 
        return(list(list(start = 1, end = len)))

    seqEnd <- seq(threads) * chunkSize + len - threads * chunkSize
    seqStart <- seqEnd - chunkSize + 1
    seqStart[1] <- 1

    lapply(seq(threads), function(y) list(start = seqStart[y], end = seqEnd[y]))
}
