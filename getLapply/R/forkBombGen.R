#'forkBombGen
#'
#'A function generator to simplify parallelisation.
#'@param FUN a single argument function to parallelise.
#'@param COLLATEFUN a function used to collate results. Default is list()
#'@return When run with list() as the COLLATEFUN() parameter, a list per
#'thread run, of lists per argument run through FUN().
#'@export
forkBombGen <- function(FUN, COLLATEFUN = list) {
    function(x) {
        LAPPLYFUN = getLapply()
        thr <- getSensibleThreads()
        l <- length(x)
        chunkSize <- l%/%thr
        if (chunkSize < 1 || thr < 2) 
            return(COLLATEFUN(do.call(COLLATEFUN, lapply(seq_along(x), function(z) FUN(x[z])))))
        seqEnd <- (seq(thr) * chunkSize) + l - (thr * chunkSize)
        seqStart <- 1 - chunkSize + seqEnd
        seqStart[1] <- 1
        # print(cbind(seqStart, seqEnd))
        do.call(COLLATEFUN, LAPPLYFUN(seq(thr), function(y) {
            do.call(COLLATEFUN, lapply(seq(seqStart[y], seqEnd[y]), function(z) {
                FUN(x[z])
            }))
        }))
    }
}
