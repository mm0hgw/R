#'urandCap
#'@description Generate random integers in the range 1:cap
#'@param cap a 'numeric' or 'bigz' positive integer cap
#'@param n a 'numeric' positive integer number of observations to produce
#'@export
urandCap <- function(cap, n = 1, ...) {
    stopifnot(length(cap) == 1)
    stopifnot(length(n) == 1)
    stopifnot(cap%%1 == 0)
    stopifnot(n%%1 == 0)
    stopifnot(cap > 0)
    stopifnot(n > 0)
    UseMethod("urandCap", cap)
}

#'@method urandCap default
urandCap.default <- function(cap, n = 1, ...) {
    sample.int(cap, n, ...)
}

#'@importFrom gmp as.bigz urand.bigz
#'@method urandCap bigz
urandCap.bigz <- function(cap, n = 1, replace = F, ...) {
    if (replace == F) 
        stopifnot(n <= cap)
    out <- as.bigz(vector())
    while (length(out) < n) {
        out2 <- as.bigz(vector())
        n2 <- n - length(out)
        while (length(out2) < n2) {
            debugCat("inner: n2:", n2, "\n")
            out2 <- c(out2, urand.bigz(n2 - length(out2), ceiling(log(cap, 2)), ...))
            debugPrint(out2)
            out2 <- out2[out2 > as.bigz(0) & out2 <= cap]
            debugPrint(out2)
        }
        debugCat("outer: n:", n, "\n")
        out <- c(out, out2)
        debugPrint(out)
        if (replace == F) {
            out <- unique(out)
        }
        debugPrint(out)
    }
    out
}

#'sampleCombo
#'@description
#'Given n,k,sampleSize, produce an ultraCombo object of that
#'n,k loaded with samples defined by sampleSize.
#'@inheritParams gmp::chooseZ
#'@param sampleSize an integer defining the size of sample to draw. Default 1000.
#'@export
sampleCombo <- function(n, k, sampleSize = 1000) {
    ultraCombo(urandCap(superChoose(n, k), sampleSize), n, k)
}
