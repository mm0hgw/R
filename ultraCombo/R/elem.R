# Generate an Element handler for combnGen
combnGenElemGenR <- function(p) {
    debugCat("combnGenElemGenR", p$indexType, p$n, p$k)
    function(index) {
        debugCat("combnGenElemR", p$indexType, p$n, p$k, index)
        out <- rep(0, p$k)
        if (p$invert == TRUE) {
            i <- p$ifun(p$imirror - index)
            debugCat("combnGenElemR", "inverted index:", index)
        } else {
            i <- p$ifun(index)
        }
        i <- n<-p$n
        j <- k<-p$k
        oldch <- p$ch
        if (p$indexType == "bigz") {
            while ((j > 0) && (oldch > integer.precision.limit)) {
                while (i > (ch <- (oldch * i)/j)) {
                  i <- i - ch
                  oldch <- oldch - ch
                  i <- i - 1
                }
                out[k - j + 1] <- n - i + 1
                oldch <- ch
                i <- i - 1
                j <- j - 1
            }
            i <- as.numeric(i)
            oldch <- as.numeric(oldch)
        }
        while (j > 0) {
            while (i > (ch <- (oldch * i)/j)) {
                i <- i - ch
                oldch <- oldch - ch
                i <- i - 1
            }
            out[k - j + 1] <- n - i + 1
            oldch <- ch
            i <- i - 1
            j <- j - 1
        }
        out[p$k] <- out[p$k - 1] + as.integer(i)
        debugCat("combnGenElemR", paste(out, collapse = ","))
        if (p$invert == TRUE) {
            setdiff(seq(p$n), out)
        } else {
            out
        }
    }
}

# generate an element handler for revCombnGen
revCombnGenElemGenR <- function(p) {
    function(x) {
        debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
        k <- length(x)
        stopifnot(k == p$k)
        invert <- FALSE
        if (k > p$n%/%2) {
            invert <- TRUE
            x <- setdiff(seq(p$n), x)
            debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
            k <- length(x)
        }
        oldch <- p$ch
        i <- p$n
        j <- k
        out <- 0
        pl <- k - 1
        p <- 0
        while (p < pl) {
            ql <- x[p + 2] - x[p + 1] - 1
            q <- 0
            while (q < ql) {
                ch <- (oldch * j)/i
                out <- out + ch
                oldch <- olch - ch
                i <- i - 1
                q <- q + 1
            }
            ch <- (oldch * j)/i
            oldch <- ch
            i <- i - 1
            j <- j - 1
            p <- p + 1
        }
        if (invert == TRUE) {
            out <- setdiff(seq(p$n), out)
        }
        out
    }
}
