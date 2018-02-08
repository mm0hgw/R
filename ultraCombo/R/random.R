#'urandCap
#'@description Generate random integers in the range 1:cap
#'@param cap a 'numeric' or 'bigz' positive integer cap
#'@param n a 'numeric' positive integer number of observations to produce
#'@export
urandCap <- function(cap, n = 1) {
    stopifnot(length(cap) == 1)
    stopifnot(length(n) == 1)
    stopifnot(cap%%1 == 0)
    stopifnot(n%%1 == 0)
    stopifnot(cap> 0)
    stopifnot(n> 0)
    UseMethod("urandCap", cap)
}

#'@importFrom stats runif
#'@method urandCap default
urandCap.default <- function(cap, n = 1) {
    ceiling(runif(n, 0, cap))
}

#'@importFrom gmp urand.bigz
#'@method urandCap bigz
urandCap.bigz <- function(cap, n = 1) {
    sapply(seq(n), function(i) {
        out <- 0
        while (out <= 0 || out > cap) {
            out <- urand.bigz(1, ceiling(log(cap, 2)))
        }
        out
    })
}
