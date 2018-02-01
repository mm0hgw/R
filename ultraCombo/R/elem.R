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
        i <- p$n 
        j <- p$k 
        oldch <- ch <- p$ch
        while ((j > 0) && (ch > integer.precision.limit)) {
         while(i > (ch <- (oldch * i) / j)){
         i <- i -  ch
         oldch <- oldch - ch
         i <- i - 1
         }
         out[k-j+1] <- n-i+1
         oldch <- ch
         i <- i-1
         j<-j-1
        }
        i <- as.numeric(i)
        while (j > 0) {
         while(i > (ch <- (oldch * i) / j)){
         i <- i -  ch
         oldch <- oldch - ch
         i <- i - 1
         }
         out[k-j+1] <- n-i+1
         oldch <- ch
         i <- i-1
         j<-j-1
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
        invert <- FALSE
        if (k > p$n%/%2) {
            invert <- TRUE
            x <- setdiff(seq(p$n), x)
            debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
            k <- length(x)
        }
        z <- c(0, x, p$n + 1)
        v <- vector()
        y <- length(z)
        for (i in seq(2, y)) {
            v <- c(z[i] - z[i - 1] - 1, v)
        }
        debugCat("revCombnGenElemR", "v:", paste(collapse = ",", v))
        out <- 1 + v[2]
        cn <- vector()
        ck <- vector()
        for (j in seq(length(v))) {
            if (j > 2) {
                if (v[j] > 0) {
                  offset <- j - 2 + sum(v[seq(j - 1)])
                  cn <- c(cn, seq(v[j]) + offset)
                  ck <- c(ck, rep(j - 2, v[j]))
                }
            }
            out <- sum(c(1, v[2], superChoose(cn, ck)))
        }
        if (invert == TRUE) {
            out <- 1 + superChoose(p$n, k) - out
        }
        debugCat("revCombnGenElemR", "out:", out)
        debugCat("revCombnGenElemR", "test:", paste(collapse = ",", combnG(out, p$n, 
            k)))
        out
    }
}

# Generate an Element handler for combnGen
combnGenElemGenC <- function(p) {
    debugCat("combnGenElemGenC", p$indexType, p$n, p$k)
    function(index) {
        debugCat("combnGenElemC", p$indexType, p$n, p$k, index)
        if (p$invert == TRUE) {
            index <- p$imirror - index
            debugCat("combnGenElemC", "inversed index:", index)
        }
        out <- combnGenElemRcpp(index, p$n, p$k, p$ch)
        if (p$invert == TRUE) {
            out <- setdiff(seq(p$n), out)
        }
        debugCat("combnGenElemC", paste(collapse = ",", out))
        out
    }
}

# Generate an Element handler for combnGen
revCombnGenElemGenC <- function(p) {
    debugCat("revCombnGenElemGenC", p$indexType, p$n)
    function(x) {
        debugCat("revCombnGenElemC", p$indexType, p$n, ":", paste(collapse = ",", 
            x))
        k <- length(x)
        invert <- FALSE
        if (k > p$n%/%2) {
            invert <- TRUE
            x <- setdiff(seq(p$n), x)
            debugCat("revCombnGenElemR", p$n, ":", paste(collapse = ",", x))
            k <- length(x)
        }
        out <- revCombnGenElemRcpp(x, p$n)
        if (invert == TRUE) {
            out <- 1 + superChoose(p$n, k) - out
        }
        debugCat("revCombnGenElemC", "out", out)
        out
    }
}

