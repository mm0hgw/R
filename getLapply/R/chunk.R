#'chunk
#'Trying to set both threads and chunkSize will return an error. 
#'If you know both, you don't need this.
#'@param len the length if the index range to chunk
#'@param chunkSize the size of chunks to create
#'@param threads the number of chunks to create
#'@return a list (even if there's just one) of lists of (from,to)  
#'arguments useable via do.call(seq,x)
#'@export
chunk <- function(len, chunkSize = NULL, threads = NULL) {
    stopifnot(length(len) == 1 && len%%1 == 0)
    if (!is.null(chunkSize)) {
        stopifnot(is.null(threads))
        stopifnot(chunkSize%%1 == 0 && chunkSize > 0)
        threads <- len%/%chunkSize
    } else {
        if (!is.null(threads)) {
            stopifnot(threads%%1 == 0 && threads > 0)
        } else {
            threads <- getSensibleThreads()
        }
        chunkSize <- len%/%threads
    }

    if (len < chunkSize)
        return(list(list(from = 1, to = len)))

    seqEnd <- seq(threads) * chunkSize + len - threads * chunkSize
    seqStart <- seqEnd - chunkSize + 1
    seqStart[1] <- 1

    lapply(seq(threads), function(y) list(from = seqStart[y], to = seqEnd[y]))
}
