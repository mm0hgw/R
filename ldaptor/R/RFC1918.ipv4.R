#'@method valid ipv4
valid.ipv4 <- function(x) {
    y <- as.numeric(x)
    if (is.na(y)) 
        return(FALSE)
    if (y < 0) 
        return(FALSE)
    if (y > ipv4.allOnes) 
        return(FALSE)
    return(TRUE)
}

#'@method as ipv4
as.ipv4 <- function(x) {
    UseMethod("as.ipv4", x)
}

#'@method as.ipv4 default
as.ipv4.default <- function(x) {
    y <- as.numeric(x)
    if (valid.ipv4(y)) {
        class(y) <- "ipv4"
        return(y)
    }
    stop(x)
}

#' ipv4
#' @description a container for a valid RFC1918 IPv4 address
#' @param '...' 4 'numeric' or 'integer' type address identifiers
#'@export
ipv4 <- function(x, ...) {
    UseMethod("ipv4", x)
}

#'@method ipv4 integer
ipv4.integer <- function(x, ...) {
    arg <- c(unlist(x), unlist(list(...)))
    stopifnot(length(arg) >= 4)
    vec <- head(arg, n = 4)
    as.ipv4(vec2ip(vec))
}

#'@method ipv4 character
ipv4.character <- function(x, ...) {
    ipv4(as.integer(strsplit(x, "\\.")[[1]]))
}

#'@method ipv4 numeric
ipv4.numeric <- ipv4.integer

#'@method ipv4 list
ipv4.list <- ipv4.integer

#'@method ipv4 ipv4
ipv4.ipv4 <- function(x, ...) {
    if (!valid.ipv4(x)) 
        stop(paste("ipv4 set to", x))
    x
}

#'@method is ipv4
is.ipv4 <- function(x) {
    inherits(x, "ipv4")
}

vec2ip <- function(vec) {
    sum(sapply(seq(4), function(i) {
        j <- 2^((4 - i) * 8)
        vec[i] * j
    }))
}

ip2vec <- function(ip) {
    sapply(seq(4), function(i) {
        j <- 2^((4 - i) * 8)
        (ip%/%j)%%256
    })
}

#'@method print ipv4
print.ipv4 <- function(x, ...) {
    cat(format(x), "\n", ...)
}

#'@method format ipv4
format.ipv4 <- function(x, ...) {
    paste(collapse = ".", ip2vec(x))
}

#'@method + ipv4
"+.ipv4" <- function(e1, e2) {
    as.ipv4(as.numeric(e1) + e2)
}

#'@method - ipv4
"-.ipv4" <- function(e1, e2) {
    as.ipv4(as.numeric(e1) - e2)
}

#'@import bit
#'@method as.ipv4 bit
as.ipv4.bit <- function(x, ...) {
    if (length(x) != 32) 
        stop("32 bits to an ipv4")
    as.ipv4(sum(sapply(seq(32), function(i) {
        if (x[i]) {
            2^(i - 1)
        } else {
            0
        }
    })))
}

#'@import bit
#'@method as.bit ipv4
as.bit.ipv4 <- function(x, ...) {
    out <- bit::bit(32)
    i <- 32
    while (i >= 1) {
        j <- 2^(i - 1)
        if (x >= j) {
            x <- x - j
            out[i] <- TRUE
        }
        i <- i - 1
    }
    out
}

