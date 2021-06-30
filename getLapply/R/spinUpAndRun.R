#'spinUpAndRun
#'@export
spinUpAndRun <- function(x, FUN, targetTime = 0.1, LAPPLYFUN = getLapply(), COLLATEFUN = lapply) {
    if (getSensibleThreads() == 1) 
        return(list(FUN(x)))
    nr <- 2
    exp <- 0
    lastTime <- 0
    l <- length(x)
    flag <- FALSE
    outList <- list()
    seqEnd <- 0
    while (flag == FALSE) {
        exp <- exp + 1
        nre <- nr^exp
        seqStart <- seqEnd + 1
        seqEnd <- min(seqEnd + nre, l)
        cat("seqStart", seqStart, "seqEnd", seqEnd, "\n")
        if (targetTime < threadTime(out <- FUN(x[seq(seqStart, seqEnd)])) || seqEnd == 
            l) 
            flag <- TRUE
        outList[[exp]] <- out
    }
    if (seqEnd < l) {
        l2 <- l - seqEnd
        n2 <- l2%/%nre
        seqEnd2 <- (seq(n2) * nre) + seqEnd
        seqStart2 <- 1 - nre + seqEnd2
        if (seqEnd2[n2] < l) {
            seqEnd2[n2 + 1] <- l
            seqStart2[n2 + 1] <- seqEnd2[n2] + 1
            n2 <- n2 + 1
        }
        print(cbind(seqStart2, seqEnd2))
        outList2 <- LAPPLYFUN(seq_along(seqStart), function(y) {
            FUN(x[seq(seqStart2[y], seqEnd2[y])])

        })
        print(outList2)
        outList <- append(outList, outList2)
    }
    do.call(COLLATEFUN, outList)
}

threadTime <- function(expr, ...) {
    st <- system.time(expr, ...)
    out <- st[1] + st[2]
    # print(out)
    out
}
