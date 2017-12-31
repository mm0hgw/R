#'@method is ldapkvlist
is.ldapkvlist <- function(x) {
    inherits(x, "ldapkvlist")
}

#'@method valid ldapkvlist
valid.ldapkvlist <- function(x) {
    if (!all(sapply(x, is.ldapkv))) {
        return(FALSE)
    }
    return(TRUE)
}

#'ldapkvlist
#'@param x 'list' of 'ldapkv' objects
#'@export
ldapkvlist <- function(x) {
    if (!valid.ldapkvlist(x)) 
        stop(match.call())
    class(x) <- "ldapkvlist"
    x
}

#'@method format ldapkvlist
format.ldapkvlist <- function(x, ...) paste(collapse = "\n", sapply(x, format))

#'@method print ldapkvlist
print.ldapkvlist <- function(x, ...) cat(format(x, ...), "\n")

#'@method + ldapkvlist
"+.ldapkvlist" <- function(e1, e2) UseMethod("+.ldapkvlist", e2)

# @method +.ldapkvlist ldapkv
"+.ldapkvlist.ldapkv" <- function(e1, e2) ldapkvlist(append(e1, list(e2)))

# @method +.ldapkvlist ldapkvlist
"+.ldapkvlist.ldapkvlist" <- function(e1, e2) ldapkvlist(append(e1, e2))

# @method +.ldapkvlist list
"+.ldapkvlist.list" <- function(e1, e2) ldapkvlist(append(e1, e2))


