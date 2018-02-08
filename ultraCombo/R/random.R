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

#'@importFrom gmp urand.bigz
#'@method urandCap bigz
urandCap.bigz <- function(cap, n = 1, replace = F, ...) {
    out <- as.bigz(vector())
    while (length(out) < n) {
        out <- append(out, sapply(seq(n - length(out)), function(i) {
            out2 <- 0
            while (out2 <= 0 || out2 > cap) {
                out2 <- urand.bigz(1, ceiling(log(cap, 2)), ...)
            }
            out2
        }))
        if (replace == F) {
            out <- unique(out)
        }
        print(out)
    }
    out
}
