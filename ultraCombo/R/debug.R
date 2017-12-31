debugFlag <- FALSE

debugCat <- function(...) {
    if (debugFlag == TRUE) {
        cat(paste(..., "\n"))
    }
}

debugPrint <- function(x, ...) {
    if (debugFlag == TRUE) {
        print(x, ...)
    }
    invisible(x)
}
