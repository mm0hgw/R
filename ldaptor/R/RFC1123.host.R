#' hostname.class
#' @description a container for a valid hostname
#' @param host a 'character' identifier
#'@examples
#'hexchars <-'0123456789ABCDEF'
#'stopifnot(valid.hostname.class(vector())==FALSE)
#'stopifnot(valid.hostname.class('')==FALSE)
#'stopifnot(valid.hostname.class(paste(collapse='',rep(4,hexchars)))==FALSE)
#'stopifnot(valid.hostname.class(':')==FALSE)
#'stopifnot(valid.hostname.class(paste(collapse='',c(letters,LETTERS,'-')))==TRUE)
#'@export
hostname.class <- function(host) {
    stopifnot(valid.hostname.class(host))
    class(host) <- "hostname.class"
    host
}

#'@method is hostname.class
is.hostname.class <- function(x) {
    inherits(x, "hostname.class")
}

#'@method as hostname.class
as.hostname.class <- function(x) {
    hostname.class(x)
}

#'@importFrom valid valid
#'@method valid hostname.class
valid.hostname.class <- function(x) {
    x <- as.character(x)
    if (length(x) != 1) 
        return(FALSE)
    if (nchar(x) == 0 || nchar(x) > 63) 
        return(FALSE)
    if (gsub(RFC1123HostnameRegex, "", x) != "") 
        return(FALSE)
    return(TRUE)
}

#'@method format hostname.class
format.hostname.class <- function(x, ...) {
    x
}

#'@method print hostname.class
print.hostname.class <- function(x, ...) {
    cat(format(x, ...), "\n")
}
