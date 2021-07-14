#'chunk
#'@param len the index range to divide
#'@param threads the number of chunks to create
#'@return a list (even if there's just one) of lists of (start,end) 
#'arguments useable via do.call(seq,x)
#'@export
chunk <- function(len, threads = getSensibleThreads()) {
    stopifnot(length(len) == 1 && len%%1 != 0)
    stopifnot(length(threads) == 1 && threads%%1 != 0)

    if (threads < 2) 
        return(list(list(start = 1, end = len)))

    chunkSize <- len%/%threads
    if (chunkSize < 1) {
        chunkSize <- 1
        threads <- len
    }

    seqEnd <- seq(threads) * chunkSize + len - threads * chunkSize
    seqStart <- seqEnd - chunkSize + 1
    seqStart[1] <- 1

    lapply(seq(threads), function(y) list(start = seqStart[y], end = seqEnd[y]))
}
