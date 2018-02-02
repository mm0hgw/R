getProfile <- function(n, k) {
    # debugCat('getProfile:',n,k)
    is.valid.nk(n, k)
    if (k == 1 || k == n) {
        return(NULL)
    }
    out <- list()
    if ((out$ch <- superChoose(n, k)) < integer.precision.limit) {
        out$indexType <- "numeric"
    } else {
        if (requireNamespace("gmp")) {
            out$indexType <- "bigz"
        } else {
            stop(paste("Combination too big. Install 'gmp' package to extend range.", 
                "n:", n, "k:", k, "choose(n,k):", choose(n, k), "limit:", integer.precision.limit, 
                sep = " "))
        }
    }
    if (k > n%/%2) {
        out$invert <- TRUE
        out$imirror <- out$ch + 1
    } else {
        out$invert <- FALSE
    }
    out$n <- n
    out$k <- k
    out$ifun <- get(paste(sep = ".", "as", out$indexType))
    # debugCat('getProfile','returning ...')
    return(out)
}
