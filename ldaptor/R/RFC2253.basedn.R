#'@method valid basedn.class
valid.basedn.class <- function(x, class1 = "basedn.class") {
    if (typeof(x) != "list") {
        print("wrong type")
        return(FALSE)
    }
    if (length(x) < 2) {
        print("wrong length")
        return(FALSE)
    }
    if (!all(sapply(x, valid.ldapkv))) {
        print("wrong elements")
        return(FALSE)
    }
    if (any(sapply(x, "[", "key") != "dc")) {
        print("wrong key")
        return(FALSE)
    }
    
    return(TRUE)
}

#'@method is basedn.class
is.basedn.class <- function(x) {
    inherits(x, "basedn.class")
}

#'basedn.class
#'@description A container for a LDAP basedn
#' @param domain a valid 'domain.class'
#'@export
basedn.class <- function(x) {
    UseMethod("basedn.class", x)
}

#'@method basedn.class default
basedn.class.default <- function(x) {
    if (!valid.basedn.class(x)) 
        stop()
    class(x) <- "basedn.class"
    x
}

#'@method basedn.class domain.class
basedn.class.domain.class <- function(x) {
    dcs <- strsplit(x, "\\.")[[1]]
    out <- lapply(dcs, ldapkv, key = "dc")
    basedn.class(out)
}

#'@method basedn.class character
basedn.class.character <- function(x) basedn.class(domain.class(x))

#'@method format basedn.class
format.basedn.class <- function(x, ...) {
    paste(collapse = ",", sapply(x, format, collapse = "="))
}

#'@method print basedn.class
print.basedn.class <- function(x, ...) {
    cat(format(x, ...), "\n")
}
