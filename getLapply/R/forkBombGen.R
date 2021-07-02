#'forkBombGen
#'@export
forkBombGen <- function(FUN, LAPPLYFUN = getLapply(), COLLATEFUN = invisible) {
    function(x) {
        thr <- getSensibleThreads()
        l <- length(x)
        chunkSize <- l%/%thr
        if (chunkSize < 1) 
            return(COLLATEFUN(FUN(x)))
        seqEnd <- (seq(thr, 1) * chunkSize)
        seqStart <- 1 - chunkSize + seqEnd
        seqEnd[1] <- l
        # print(cbind(seqStart, seqEnd))
        do.call(COLLATEFUN, LAPPLYFUN(seq(thr), function(y) {
            FUN(x[seq(seqStart[y], seqEnd[y])])
        }))
    }
}
