#' domain.class
#' @description a container for a valid domaim name
#' @param domain a 'character' identifier
#'@examples
#'hexchars <-'0123456789ABCDEF'
#'library(valid)
#'ls()
#'stopifnot(valid(vector(),'domain.class')==FALSE)
#'stopifnot(valid('','domain.class')==FALSE)
#'stopifnot(valid(paste(collapse='',rep(16,hexchars)),'domain.class')==FALSE)
#'stopifnot(valid(':','domain.class')==FALSE)
#'stopifnot(valid(paste(collapse='',c(letters,,'.',LETTERS,'-')),'domain.class')==TRUE)
#'@export
domain.class <- function(x) {
    UseMethod("domain.class", x)
}

#'@method domain.class default
domain.class.default <- function(x) {
    class(x) <- "domain.class"
    stopifnot(valid(x))
    x
}

#'@method domain.class basedn.class
domain.class.basedn.class <- function(x) {
    domain.class(paste(collapse = ".", sapply(x, "[[", "value")))
}

#'@method is domain.class
is.domain.class <- function(x) {
    inherits(x, "domain.class")
}

#'@method as domain.class
as.domain.class <- function(x) {
    domain.class(x)
}

#'@importFrom valid valid
#'@method valid domain.class
valid.domain.class <- function(x, class1) {
    x <- as.character(x)
    if (length(x) != 1) 
        return(FALSE)
    if (nchar(x) == 0 || nchar(x) > 253) 
        return(FALSE)
    dcs <- strsplit(x, ".")[[1]]
    if (all(sapply(dcs, valid.hostname.class))) 
        return(FALSE)
    return(TRUE)
}

#'@method format domain.class
format.domain.class <- function(x, ...) {
    x
}

#'@method print domain.class
print.domain.class <- function(x, ...) {
    cat(format(x, ...), "\n")
}
