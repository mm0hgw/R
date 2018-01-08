
#'@method valid url.class
valid.url.class <- function(x, class2 = "url.class") {
    if (typeof(x) != "character") 
        return(F)
    if (length(x) != 5) 
        return(F)
    return(T)
}

#'url.class
#'@export
url.class <- function(x, ...) {
    UseMethod("url.class", x)
}

RFC3986Regex <- "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
urlElems <- c("\\2", "\\4", "\\5", "\\7", "\\9")

#'@method url.class default
url.class.default <- function(x, ...) {
    if (length(x) == 1) {
        x <- sapply(urlElems, gsub, pattern = RFC3986Regex, x = x)
    }
    if (valid.url.class(x) != TRUE) 
        stop()
    class(x) <- "url.class"
    names(x) <- c("service", "authority", "path", "query", "fragment")
    x
}

#'@method format url.class
format.url.class <- function(x, ...) {
    out <- paste(collapse = "", x[c(2, 3)])
    if (x[1] != "") 
        out <- paste(sep = "://", x[1], out)
    if (x[4] != "") 
        out <- paste(sep = "?", out, x[4])
    if (x[5] != "") 
        out <- paste(sep = "#", out, x[5])
    out
}

#'@method print url.class
print.url.class <- function(x, ...) {
    cat(format(x, ...), "\n")
}
