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
#'Get a lapply with an explicity unique PRNG seed.
#'@export
getSeededLapply <- function() {
    get("seededLapply", envir = getLapplyEnv)
}

#'setSeededLapply
#'@description
#'Set the served lapply with an explicity unique PRNG seed.
#'@param x a 'function' 
#'@export
setSeededLapply <- function(x = lapply) {
    stopifnot(is.function(x))
    assign("seededLapply", x, envir = getLapplyEnv)
}

#'getSensibleThreads
#'@description
#'Get the sensible number of threads to plan to run concurrently.
#'@export
getSensibleThreads <- function() {
    get("sensibleThreads", envir = getLapplyEnv)
}

#'setSensibleThreads
#'@description
#'Get the sensible number of threads to plan to run concurrently.
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
