# Generate an Element handler for combnGen
combnGenElemGenR <- function(p) {
    debugCat("combnGenElemGenR", p$indexType, p$n, p$k)
    function(x) {
        debugCat("combnGenElemR", p$indexType, p$n, p$k, x)
        oldch <- p$ch
        i <- n <- p$n
        k <- p$k
        if (p$invert == TRUE) {
            x <- p$ifun(p$imirror - x)
            k <- n - k
            debugCat("combnGenElemR", "inverted index:", x)
        } else {
            x <- p$ifun(x)
        }
        out <- rep(0, p$k)
        j <- k
        if (p$indexType == "bigz") {
            while ((j > 1) && (oldch > integer.precision.limit)) {
                while (x > (ch <- (oldch * j)/i)) {
                  debugCat("x:", x, "ch:", ch, "i:", i, "j:", j)
                  x <- x - ch
                  oldch <- oldch - ch
                  i <- i - 1
                }
                debugCat("x:", x, "ch:", ch, "i:", i, "j:", j)
                out[k - j + 1] <- n - i + 1
                oldch <- ch
                i <- i - 1
                j <- j - 1
            }
            x <- as.numeric(x)
            oldch <- as.numeric(oldch)
        }
        debugPrint(out)
        while (j > 1) {
            while (x > (ch <- (oldch * j)/i)) {
                debugCat("x:", x, "ch:", ch, "i:", i, "j:", j)
                x <- x - ch
                oldch <- oldch - ch
                i <- i - 1
            }
            debugCat("x:", x, "ch:", ch, "i:", i, "j:", j)
            out[k - j + 1] <- n - i + 1
            oldch <- ch
            i <- i - 1
            j <- j - 1
        }
        debugPrint(out)
        out[k] <- out[k - 1] + as.integer(x)
        debugPrint(out)
        if (p$invert == TRUE) {
            out <- setdiff(seq(p$n), out)
        }
        debugCat("combnGenElemR", paste(out, collapse = ","))
        out
    }
}

# generate an element handler for revCombnGen
revCombnGenElemGenR <- function(p) {
    function(x) {
        debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
        k <- length(x)
        oldch <- superChoose(p$n, k)
        invert <- FALSE
        if (k > p$n%/%2) {
            invert <- TRUE
            x <- setdiff(seq(p$n), x)
            debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
            k <- length(x)
            imirror <- oldch + 1
        }
        i <- p$n
        j <- k
        out <- x[k] - x[k - 1]
        r <- 0
        x <- c(0, x)
        while (r < k - 1) {
            debugCat("r:", r, k - 1, out, i, j)
            ql <- x[r + 2] - x[r + 1] - 1
            q <- 0
            while (q < ql) {
                debugCat("q:", q, ql, out, i, j)
                ch <- (oldch * j)/i
                out <- out + ch
                oldch <- oldch - ch
                i <- i - 1
                q <- q + 1
            }
            ch <- (oldch * j)/i
            oldch <- ch
            i <- i - 1
            j <- j - 1
            r <- r + 1
        }
        if (invert == TRUE) {
            out <- imirror - out
        }
        ultraCombo(out, p$n, k)
    }
}
