#'@method valid ipv4.subnetlist
valid.ipv4.subnetlist <- function(x) {
    all(sapply(x, is.ipv4.subnet))
}

#'ipv4.subnetlist
#' @param x a 'character' filename like '/etc/networks' 
#' or a 'character' vector like scan('/etc/networks',what='character',sep='\\n')
#' or a 'list' of 'ipv4.subnet' objects
#'@export
ipv4.subnetlist <- function(x, ...) {
    UseMethod("ipv4.subnetlist", x)
}

#'@method ipv4list list
ipv4.subnetlist.list <- function(x, ...) {
    if (!valid.ipv4.subnetlist(x)) 
        stop(x)
    class(x) <- "ipv4.subnetlist"
    x
}

#'@method ipv4.subnetlist character
ipv4.subnetlist.character <- function(x, ...) {
    if (length(x) == 1 && file.exists(x)) {
        x <- scan(x, what = "character", sep = "\n")
    }
    print(x)
    x <- grep("^#", x, invert = TRUE, value = TRUE)
    print(x)
    x <- strsplit(x, "([[:space:]])")
    print(x)
    x <- lapply(x, function(y) y[y != ""])
    print(x)
    
    templist <- lapply(x, function(y) {
        list(subnet = ipv4.subnet(y[1]), name = paste(collapse = " ", y[-1]))
    })
    out <- lapply(templist, "[[", "subnet")
    names(out) <- sapply(templist, "[[", "name")
    valid.ipv4.subnetlist(out)
}

#'@method print ipv4.subnetlist 
print.ipv4.subnetlist <- print.ipv4list

subnetkvlist <- structure(list(structure(c("objectClass", "top"), class = "ldapkv"), 
    structure(c("objectClass", "dhcpSubnet"), class = "ldapkv"), structure(c("objectClass", 
        "dhcpOptions"), class = "ldapkv")), class = "ldapkvlist")

dhcpkvlist <- structure(list(structure(c("objectClass", "top"), class = "ldapkv"), 
    structure(c("objectClass", "organisationalUnit"), class = "ldapkv"), structure(c("description", 
        "DHCP Servers"), class = "ldapkv")), class = "ldapkvlist")

dhcpserverkvlist <- structure(list(structure(c("objectClass", "top"), class = "ldapkv"), 
    structure(c("objectClass", "dhcpServer"), class = "ldapkv")), class = "ldapkvlist")

dhcpservicekvlist <- structure(list(structure(c("objectClass", "top"), class = "ldapkv"), 
    structure(c("objectClass", "dhcpService"), class = "ldapkv")), class = "ldapkvlist")

dhcpskey <- structure(list(structure(c(key = "ou", value = "dhcp"), class = "ldapkv")), 
    class = "ldapkvlist")

subnetskey <- structure(list(structure(c("cn", "config"), class = "ldapkv"), structure(c("ou", 
    "dhcp"), class = "ldapkv")), class = "ldapkvlist")

#'@method format ipv4.subnetlist 
format.ipv4.subnetlist <- function(x, basedn = NULL, dhcpStatements = list(), ...) {
    if (!missing(basedn)) 
        UseMethod("format.ipv4.subnetlist", basedn)
    paste(collapse = "\n", paste(sapply(x, format), names(x), sep = "\t"))
}

#'@method format.ipv4.subnetlist basedn.class
format.ipv4.subnetlist.basedn.class <- function(x, basedn, dhcpStatements = list(), 
    ...) {
    domain <- domain.class(basedn)
    serverquery <- lapply(seq_along(x), function(i) {
        if (names(x)[i] == basedn[[1]][2]) 
            return(list())
        pkey <- ldapkv("cn", names(x)[i])
        ldapquery(list(pkey, basedn, dhcpskey, dhcpserverkvlist))
    })
    servicequery <- lapply(seq_along(x), function(i) {
        if (names(x)[i] == basedn[[1]][2]) 
            return(list())
        pkey <- ldapkv("cn", paste(sep = "", names(x)[i], "config"))
        pdn <- list(ldapkv("dhcpPrimaryDN", dn(serverquery[[i]])["value"]))
        kvlist <- c(dhcpservicekvlist, pdn, dhcpStatements)
        ldapquery(list(pkey, basedn, dhcpskey, kvlist))
    })
    serverquery <- lapply(seq_along(x), function(i) {
        if (names(x)[i] == basedn[[1]][2]) 
            return(list())
        pkey <- ldapkv("cn", names(x)[i])
        ldapquery(list(pkey, basedn, dhcpskey, dhcpserverkvlist + ldapkv("dhcpServiceDN", 
            dn(servicequery[[i]])["value"])))
    })
    subnetquery <- lapply(seq_along(x), function(i) {
        if (names(x)[i] == basedn[[1]][2]) 
            return(list())
        subdomain <- paste(sep = ".", names(x)[i], domain)
        router <- x[[i]]$ip + 1
        nm <- netmask(x[[i]])
        bc <- broadcast(x[[i]])
        pkey <- ldapkv("cn", format(x[[i]]$ip))
        dhcpNetMask <- ldapkv("dhcpNetMask", x[[i]]$mask)
        dhcpRange <- ldapkv("dhcpRange", paste(format(router + 1), format(bc - 1)))
        dhcpOptions <- lapply(c(paste("subnet-mask", format(nm)), paste("broadcast-address", 
            format(bc)), paste("routers", format(router)), paste("domain-name-servers", 
            format(router)), paste(sep = "", "domain-name \"", subdomain, "\"")), 
            ldapkv, key = "dhcpOptions")
        kvlist <- subnetkvlist + dhcpNetMask + dhcpRange + dhcpStatements + dhcpOptions
        skey <- ldapkvlist(c(list(ldapkv("cn", paste(sep = "", names(x)[i], "config"))), 
            dhcpskey))
        ldapquery(list(pkey, basedn, skey, kvlist))
    })
    out <- c(serverquery, servicequery, subnetquery)
    ldapquerylist(out[sapply(out, valid.ldapquery)])
}
