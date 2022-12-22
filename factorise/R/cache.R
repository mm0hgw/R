#'resetCache
#'@export
resetCache <- function() {
    assign("cap", 1, envir = primesEnv)
    assign("primes", vector(), envir = primesEnv)
}

#'setChunkSize
#'@export
setChunkSize <- function(x) {
    assign("chunkSize", x, envir = primesEnv)
}

