#'@method valid ldapkv
valid.ldapkv <- function(x) {
    if ("character" != typeof(x)) 
        return(FALSE)
    if (inherits(x, "RFC2253Character")) 
        return(FALSE)
    if (length(x) != 2) 
        return(FALSE)
    return(TRUE)
}

#' ldapkv
#' @description a container for a valid LDAP key/value pair
#' @param ... a valid 'RFC2253Character' key/value pair
#'@export
ldapkv <- function(key, value) {
    out <- RFC2253Character(c(key, value))
    names(out) <- c("key", "value")
    class(out) <- "ldapkv"
    stopifnot(valid.ldapkv(out))
    out
}

#'@method is ldapkv
is.ldapkv <- function(x) {
    inherits(x, "ldapkv")
}

#'@method format ldapkv
format.ldapkv <- function(x, collapse = ": ", ...) {
    paste(collapse = collapse, x)
}

#'@method print ldapkv
print.ldapkv <- function(x, ...) {
    cat(format(x, ...), "\n")
}
