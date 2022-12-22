#' getPrimes
#'@param x a 'numeric' integer describing the maximum desired prime.
#' @export
getPrimes <- function(x) {
    # catp('getPrimes', x)
    if (length(x) > 1)
        return(lapply(x, getPrimes))
    stopifnot(x <= precisionLimit)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    cap <- get("cap", envir = primesEnv)
    primes <- get("primes", envir = primesEnv)
    capreq <- floor(sqrt(x))
    if (cap < capreq) {
        primes <- getPrimes(capreq)
        cap <- capreq
    }
    # chunkSize <- get('chunkSize', envir = primesEnv) if (x - cap > chunkSize)
    # { j <- seq(cap, x, by = chunkSize) primes <- lapply(j,
    # getPrimes)[[length(j)]] }
    if (cap < x) {
        r <- setdiff(primeGen(cap, x), primes)
        primes <- c(primes, r)
        cat(paste("Extended cache from", cap, "to", x, "and found", length(r), "new primes ",
            length(primes), "in cache\n"))
        cap <- x
        assign("primes", primes, envir = primesEnv)
        assign("cap", cap, envir = primesEnv)
    }
    primes[primes <= x]
}

#'primesN
#'@param x a 'numeric' indexing the primes cache
#'@export
primesN <- function(x) {
    # catp('primesN', x)

    if (length(x) > 1)
        return(lapply(x, primesN))
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    primes <- get("primes", envir = primesEnv)
    while (length(primes) < x) {
        d <- x - length(primes)
        primes <- getPrimes(get("cap", envir = primesEnv) + 20 * d)
    }
    primes[x]
}

nonPrimeGen <- function(from, to) {
    # catp('nonPrimeGen', from, to)

    function(n) {
        # catp('nonPrime', n)
        if (n^2 + n > to) {
            return(n^2)
        }
        fn <- from + n - from%%n
        if (fn < n^2) {
            fn <- n^2
        }
        tn <- to - to%%n
        if (tn < fn) {
            return(vector())
        }
        seq(fn, tn, by = n)
    }
}

#' @importFrom getLapply getLapply
#' @importFrom multiUnion multiUnion
primeGenThread <- function(fromto) {
    from <- fromto$from
    to <- fromto$to
    # catp('primeGenThread', from, to)
    if (to <= from) {
        return(vector())
    }
    fun <- nonPrimeGen(from, to)
    p <- getPrimes(floor(sqrt(to)))
    np <- do.call(multiUnion::multiUnion, lapply(p, fun))
    setdiff(seq(from + 1, to), np)
}

#' @importFrom getLapply getLapply chunk
primeGen <- function(from, to) {
    catp("primeGen", from, to)
    chunkSize <- get("chunkSize", envir = primesEnv)
    r <- getLapply::chunk(from, to, chunkSize = chunkSize)
    cat(paste("from", from, "to", to, ":", to - from, "candidates... Running", length(r),
        "jobs\n"))
    LAPPLYFUN <- getLapply::getLapply()
    l <- LAPPLYFUN(r, primeGenThread)
    out <- do.call(c, l)
    b <- length(out)
    cat(paste(b, "found in", to - from, "candidates", sprintf("%0.2f%%", 100 * b/(to -
        from)), "\n"))
    return(out)
}

#' factorise
#'@param x a 'numeric' integer describing a number to factorise.
#'@export
factorise <- function(x) {
    # catp('factorise', x)
    if (length(x) > 1)
        return(lapply(x, factorise))
    stopifnot(x <= precisionLimit)
    stopifnot(x%%1 == 0)
    stopifnot(x > 0)
    p <- getPrimes(floor(sqrt(x)))
    p[x%%p == 0]
}

catp <- function(...) {
    cat(paste(..., "\n"))
}
