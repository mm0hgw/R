#'@method valid ipv4.subnetmask
valid.ipv4.subnetmask <- function(x) {
    if (length(x) != 1) 
        return(FALSE)
    if (is.na(x)) 
        return(FALSE)
    if (x < 0 || x > 30) 
        return(FALSE)
    return(TRUE)
    
}

#'@method valid ipv4.subnet
valid.ipv4.subnet <- function(x) {
    if (typeof(x) != "list") 
        return(FALSE)
    if (!valid.ipv4(x$ip)) 
        return(FALSE)
    if (!valid.ipv4.subnetmask(x$mask)) 
        return(FALSE)
    ipbit <- as.bit(x$ip)
    bitsum <- sum(head(ipbit, n = 32 - x$mask))
    if (bitsum != 0) 
        return(FALSE)
    return(TRUE)
}

#' ipv4.subnet
#' @description a container for a valid RFC1918 IPv4 subnet
#' @param '...' 5 'numeric' or 'integer' type subnet identifiers
#'@export
ipv4.subnet <- function(x, ...) {
    UseMethod("ipv4.subnet", x)
}

#'@method ipv4.subnet integer
ipv4.subnet.integer <- function(x, ...) {
    arg <- c(unlist(x), unlist(list(...)))
    if (any(is.na(arg))) 
        stop("NA input")
    if (length(arg) < 5) 
        stop(paste("too few arguments,n=", length(arg), sep = ""))
    if (!valid.ipv4.subnetmask(arg[5])) 
        stop(paste("bad subnet size:", arg[5]))
    out <- list(ip = ipv4(arg[1:4]), mask = arg[5])
    class(out) <- c("ipv4.subnet")
    out
}

#'@method ipv4.subnet character
ipv4.subnet.character <- function(x, ...) {
    ipv4.subnet(as.integer(strsplit(x, "([\\./]).")[[1]]))
}

#'@method ipv4.subnet numeric
ipv4.subnet.numeric <- ipv4.subnet.integer

#'@method ipv4.subnet list
ipv4.subnet.list <- ipv4.subnet.integer

#'@method ipv4.subnet ipv4
ipv4.subnet.ipv4 <- function(x, ...) {
    out <- list()
    out$ip <- x
    out$mask <- c(...)
    if (!valid.ipv4.subnet(out)) 
        stop(match.call())
    class(out) <- "ipv4.subnet"
    out
}

#'@method ipv4.subnet ipv4.subnet
ipv4.subnet.ipv4.subnet <- function(x, ...) {
    if (!valid.ipv4.subnet(x)) 
        stop(match.call())
    x
}

#'@method print ipv4.subnet
print.ipv4.subnet <- function(x, ...) cat(format(x, ...), "\n")

#'@method format ipv4.subnet
format.ipv4.subnet <- function(x, ...) paste(sep = "/", format(x$ip, ...), x$mask)

#'netmask
#'@param subnet an 'ipv4.subnet' object
#'@export
netmask <- function(subnet) as.ipv4(ipv4.allOnes - (2^(32 - subnet$mask) - 1))

#'broadcast
#'@param subnet an 'ipv4.subnet' object
#'@export
broadcast <- function(subnet) as.ipv4(subnet$ip + 2^(32 - subnet$mask) - 1)

#'@method as ipv4.subnet
as.ipv4.subnet <- function(x) {
    y <- as.numeric(x)
    if (valid.ipv4.subnet(y)) {
        class(y) <- "ipv4.subnet"
        return(y)
    }
    stop(x)
}

#'@method is ipv4.subnet
is.ipv4.subnet <- function(x) inherits(x, "ipv4.subnet")
