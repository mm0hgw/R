#'@method is ldapquerylist
is.ldapquerylist <- function(x) {
    inherits(x, "ldapquerylist")
}

#'@method valid ldapquerylist
valid.ldapquerylist <- function(x) {
    if (!all(sapply(x, is.ldapquery))) {
        return(FALSE)
    }
    return(TRUE)
}

#'ldapquerylist
#'@param x a 'list' of 'ldapquery' objects
#'@export
ldapquerylist <- function(x) {
    if (!valid.ldapquerylist(x)) 
        stop(match.call())
    class(x) <- "ldapquerylist"
    x
}

#'@method format ldapquerylist
format.ldapquerylist <- function(x, ...) {
    paste(collapse = "\n", sapply(x, format))
}

#'@method print ldapquerylist
print.ldapquerylist <- function(x, ...) {
    cat(format(x, ...), "\n")
}

#'@method + ldapquerylist
"+.ldapquerylist" <- function(e1, e2) {
    UseMethod("+.ldapquerylist", e2)
}

# @method +.ldapquerylist ldapquery
"+.ldapquerylist.ldapquery" <- function(e1, e2) {
    ldapquerylist(append(e1, list(e2)))
}

# @method +.ldapquerylist ldapquerylist
"+.ldapquerylist.ldapquerylist" <- function(e1, e2) {
    ldapquerylist(append(e1, e2))
}

