#'@method valid ldapquery
valid.ldapquery <- function(x, class1 = "ldapquery") {
    if (typeof(x) != "list") {
        print("wrong type")
        return(FALSE)
    }
    if (length(x) != 4) {
        print("wrong length")
        return(FALSE)
    }
    if (!valid.ldapkv(x[[1]])) {
        print(x[[1]])
        print("wrong pkey")
        return(FALSE)
    }
    if (!valid.basedn.class(x[[2]])) {
        print("wrong basedn")
        return(FALSE)
    }
    if (!valid.ldapkvlist(x[[3]])) {
        print("wrong skeylist")
        return(FALSE)
    }
    if (!valid.ldapkvlist(x[[4]])) {
        print("wrong kvlist")
        return(FALSE)
    }
    return(TRUE)
}

#' ldapquery
#' @description a container for a valid LDAP query
#' @param x a 'list' with the elements : \cr 
#' pkey a 'ldapkv' the primary key for the query\cr
#' basedn a valid 'basedn.class' the base dn for the query\cr
#' skeylist a 'ldapkvlist' the secondary keys for the query\cr
#' kvlist a 'ldapkvlist' the content of the query
#'@export
ldapquery <- function(x) {
    if (!valid(x, "ldapquery")) 
        stop(match.call())
    names(x) <- c("pkey", "basedn", "skeylist", "kvlist")
    class(x) <- "ldapquery"
    x
}

#'dn
#'@param x a 'ldapquery'
#'@return a 'ldapkv' containing the distinguished name of the 'ldapquery'
#'@export
dn <- function(x) ldapkv("dn", paste(collapse = ",", sapply(c(list(x$pkey), x$skeylist, 
    x$basedn), format, collapse = "=")))

#'@method format ldapquery
format.ldapquery <- function(x, ...) paste(collapse = "\n", c(format(dn(x)), sapply(c(list(x$pkey), 
    x$kvlist), format), ""))

#'@method print ldapquery
print.ldapquery <- function(x, ...) cat(format(x, ...), "\n")

#'@method is ldapquery
is.ldapquery <- function(x) inherits(x, "ldapquery")

#'@method + ldapquery
"+.ldapquery" <- function(e1, e2) UseMethod("+.ldapquery", e2)

#'@method +.ldapquery ldapkv
"+.ldapquery.ldapkv" <- function(e1, e2) {
    e1$kvlist <- e1$kvlist + e2
    e1
}
