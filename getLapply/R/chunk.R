#'chunk
#'Trying to set both threads and chunkSize will return an error. 
#'If you know both, you don't need this.
#'@param from the start of the index range to chunk
#'@param to the end of the index range to chunk
#'@param chunkSize the size of chunks to create
#'@param threads the number of chunks to create
#'@return a list (even if there's just one) of lists of (from,to)  
#'arguments useable via do.call(seq,x)
#'@export
chunk <- function(from = 1, to, chunkSize = NULL, threads = NULL) {
    stopifnot(length(from) == 1 && from%%1 == 0)
    stopifnot(length(to) == 1 && to%%1 == 0)
    stopifnot(from < to)
    len <- to - from
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
        return(list(list(from = from, to = to)))

    seqEnd <- seq(threads) * chunkSize + to - threads * chunkSize
    seqStart <- seqEnd - chunkSize + 1
    seqStart[1] <- from

    z <- lapply(seq(threads), function(y) list(from = seqStart[y], to = seqEnd[y]))
    out <- z[sapply(z, function(x) x$from < x$to)]
    out
}
