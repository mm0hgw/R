function (x, basedn = NULL, dhcpStatements = list(), ...) 
{
    n <- names(x)
    domain <- paste(collapse = ".", sapply(basedn, "[[", "value"))
    ldapquerylist(lapply(seq_along(x), function(i) {
        router <- x[[i]]$ip + 1
        pkey <- ldapkv("cn", format(x[[i]]$ip))
        dhcpNetMask <- ldapkv("dhcpNetMask", x[[i]]$mask)
        dhcpOptions <- lapply(paste(c("subnet-mask", "broadcast-address", 
            "routers", "domain-name-servers", "domain-name"), 
            c(netmask(x[[i]]), broadcast(x[[i]]), router, router, 
                domain)), ldapkv, key = "dhcpOptions")
        kvlist <- subnetkvlist + dhcpNetMask + dhcpStatements + 
            dhcpOptions
        ldapquery(pkey, basedn, hostskey, kvlist)
    }))
}
