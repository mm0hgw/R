#' ultraCombo
#'@description Create a combination object.
#'@inheritParams is.valid.nk 
#'@inheritParams is.valid.index 
#'@return A list with the following elements:\cr
#'$i The indices of the combination set.\cr
#'$n The n of the combination set.\cr
#'$k The k of the combination set.\cr
#'$Gen A function which generates combinations from the indices
#'@examples
#'n<-20
#'k<-15
#'combo<-ultraCombo(seq(choose(n,k)),n,k)
#'print(combo)
#'object.size(combo$Gen(seq(combo$len)))
#' @export
ultraCombo <- function(i, n, k) {
    is.valid.nk(n, k)
    is.valid.index(i, n, k)
    if (length(i) == 0 || max(i) < .Machine$integer.max)
        i <- as.integer(i)
    combnGen <- combnGG(n, k)
    out <- list(i = i, len = length(i), n = as.integer(n), k = as.integer(k), Gen = function(x) {
        if (any(c(x < 1, x > length(i)))) {
            stop("index out of range")
        }
        combnGen(i[x])
    }, sample = function(sampleSize = 1000) ultraCombo(i[urandCap(length(i), sampleSize)],
        n, k))
    class(out) <- "ultraCombo"
    out
}

#'[.ultraCombo
#'@param x a combo object
#'@param i index of the desired subset of combo object
#'@examples
#'n<-4
#'k<-2
#'combo<-ultraCombo(seq(choose(n,k)),n,k)
#'combo[seq(3)]
#'combo[seq(3)*2]
#'try(combo[0])
#'try(combo[7])
#'@export
"[.ultraCombo" <- function(x, i) {
    ultraCombo(x$i[i], x$n, x$k)
}

#'print.ultraCombo
#'@param x An 'ultraCombo' to print
#'@param ... ignored
#'@importFrom utils object.size
#'@method print ultraCombo
#'@export
print.ultraCombo <- function(x, ...) {
    cat(paste(sep = "\n", paste(sep = "", "ultraCombo object n=", x$n, " k=", x$k),
        paste("contains", x$len, "indices,"), paste("has size of", object.size(x),
            "bytes"), ""))
    invisible(x)
}

#'length.ultraCombo
#'@method length ultraCombo
#'@export
length.ultraCombo <- function(x, ...) {
    x$len
}
