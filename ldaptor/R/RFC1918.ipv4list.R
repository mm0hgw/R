#'@method valid ipv4list
valid.ipv4list <- function(x) {
    if (!identical(typeof(x), "list")) 
        return(FALSE)
    all(sapply(x, is.ipv4))
}

#'ipv4list
#' @param x a 'character' filename like '/etc/hosts' 
#' or a 'character' vector like scan('/etc/hosts',what='character',sep='\\n')
#' or a 'list' of 'ipv4' objects
#'@export
ipv4list <- function(x, ...) UseMethod("ipv4list", x)

#'@method ipv4list list
ipv4list.list <- function(x, ...) {
    if (!valid.ipv4list(x)) 
        stop(x)
    class(x) <- "ipv4list"
    x
}

#'@method ipv4list character
ipv4list.character <- function(x, ...) {
    if (length(x) == 1 && file.exists(x)) {
        x <- scan(x, what = "character", sep = "\n")
    }
    x <- grep("^#", x, invert = TRUE, value = TRUE)
    x <- grep("^f", x, invert = TRUE, value = TRUE)  #skip ipv6
    x <- strsplit(x, "([[:space:]])")
    templist <- lapply(x, function(y) {
        t1 <- strsplit(y[1], "\\.")[[1]]
        if (identical(length(t1), 4L)) {
            ip <- ipv4(y[1])
            name <- paste(collapse = " ", y[-1])
            list(data = TRUE, ip = ip, name = name)
        } else {
            list(data = FALSE)
        }
    })
    templist <- templist[sapply(templist, "[[", "data")]
    out <- lapply(templist, "[[", "ip")
    names(out) <- sapply(templist, "[[", "name")
    ipv4list(out)
}

#'@method format ipv4list
format.ipv4list <- function(x, basedn = NULL, ...) {
    if (!missing(basedn)) {
        UseMethod("format.ipv4list", basedn)
    }
    y <- sapply(x, format)
    paste(collapse = "\n", c(sapply(seq_along(x), function(i) {
        paste(y[i], names(x)[i], sep = "\t")
    }), ""))
}

#'@method print ipv4list 
print.ipv4list <- function(x, ...) cat(format(x, ...), "\n")

hostkvlist <- structure(list(structure(c("objectClass", "top"), class = "ldapkv"), 
    structure(c("objectClass", "ipHost"), class = "ldapkv"), structure(c("objectClass", 
        "device"), class = "ldapkv")), class = "ldapkvlist")

hostskey <- structure(list(structure(c("ou", "Hosts"), class = "ldapkv", .Names = c("key", 
    "value"))), class = "ldapkvlist")

#'@method format.ipv4list basedn.class
format.ipv4list.basedn.class <- function(x, basedn, ...) {
    n <- strsplit(names(x), " ")
    ldapquerylist(lapply(seq_along(x), function(i) {
        pkey <- ldapkv("cn", n[[i]][1])
        kvlist <- hostkvlist + ldapkv("ipHostNumber", format(x[[i]])) + lapply(n[[i]][-1], 
            ldapkv, key = "cn")
        out <- list(pkey, basedn, hostskey, kvlist)
        ldapquery(out)
    }))
}
