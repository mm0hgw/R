

UserClasses <- c("character", "character", "integer", "integer", "gecos", "character", 
    "character")

#'@method valid User.class
valid.User.class <- function(x, class1 = "User.class") {
    if (typeof(x) != "list") {
        print("wrong type")
        return(F)
    }
    if (length(x) != 7) {
        print("wrong length")
        return(F)
    }
    if (any(sapply(x, class) != UserClasses)) 
        return(F)
    if (x[[1]] == "") 
        return(F)
    if (x[[3]] < 0) 
        return(F)
    if (x[[4]] < 0) 
        return(F)
    if (x[[6]] == "") 
        return(F)
    if (x[[7]] == "") 
        return(F)
    return(T)
}

#'User.class
#'@param x an object to form a 'User.class' from
#'@export
User.class <- function(x, ...) {
    UseMethod("User.class", x)
}

#'@method User.class default
User.class.default <- function(x, ...) {
    if (length(x) == 7) 
        x <- list(as.character(x[[1]]), as.character(x[[2]]), as.integer(x[[3]]), 
            as.integer(x[[4]]), as.gecos(x[[5]]), as.character(x[[6]]), as.character(x[[7]]))
    class(x) <- "User.class"
    print(x)
    if (!valid(x)) 
        stop()
    names(x) <- c("Username", "Password", "uid", "gid", "gecos", "home", "shell")
    x
}

#'@method User.class character
User.class.character <- function(x, ...) {
    x <- as.list(strsplit(x, split = ":")[[1]])
    NextMethod()
}

#'@method is User.class
is.User.class <- function(object, ...) inherits(object, "User.class")

#'@method as.User.class default
as.User.class <- function(object, ...) User.class(object)

#'@method format User.class
format.User.class <- function(x, basedn = NULL, ...) UseMethod("format.User.class", 
    basedn)

#'@method format.User.class default
format.User.class.default <- function(x, basedn = NULL, ...) paste(collapse = ":", 
    sapply(x, format))

Userkvlist <- structure(list(structure(c("objectClass", "top"), class = "ldapkv"), 
    structure(c("objectClass", "account"), class = "ldapkv"), structure(c("objectClass", 
        "posixAccount"), class = "ldapkv")), class = "ldapkvlist")

#'@method format.User.class basedn.class
format.User.class.basedn.class <- function(x, basedn = NULL, ...) {
    realm <- toupper(domain.class(basedn))
    y <- sapply(x, format)
    pkey <- ldapkv("uid", y[1])
    skey <- list(ldapkv("ou", "People"))
    kvlist <- Userkvlist + ldapkv("cn", y[1]) + ldapkv("userPassword", paste(sep = "", 
        "{SASL}", y[1], "@", realm)) + ldapkv("uidNumber", y[3]) + ldapkv("gidNumber", 
        y[4]) + ldapkv("gecos", y[5]) + ldapkv("homeDirectory", y[6]) + ldapkv("loginShell", 
        y[7])
    format(ldapquery(list(pkey, basedn, skey, kvlist)))
}

#'@method print User.class
print.User.class <- function(x, ...) cat(format(x, ...), "\n")

#'@method valid gecos
valid.gecos <- function(x, class1 = "gecos") {
    if (typeof(x) != "character") 
        return(F)
    if (length(x) != 5) 
        return(F)
    return(T)
}

#'gecos
#'@param x an object to build a gecos object with
#'@export
gecos <- function(x, ...) {
    UseMethod("gecos", x)
}

#'@method gecos default
gecos.default <- function(x, ...) {
    class(x) <- "gecos"
    if (!valid(x)) 
        stop()
    names(x) <- c("Full Name", "Location", "Office #", "Home #", "Other")
    x
}

#'@method gecos character
gecos.character <- function(x, ...) {
    if (length(x) == 1) {
        x <- strsplit(x, split = ",")[[1]]
        if (length(x) < 5) 
            x <- c(x, rep("", 5 - length(x)))
    }
    NextMethod()
}

#'@method is gecos
is.gecos <- function(object, ...) inherits(object, "gecos")

#'@method as gecos
as.gecos <- function(object, ...) gecos(object)

#'@method format gecos
format.gecos <- function(x, ...) paste(collapse = ",", x)

#'@method print gecos
print.gecos <- function(x, ...) cat(format(x, ...), "\n")

#'@method valid User.list
valid.User.list <- function(x, class1 = "User.list") {
    if (typeof(x) != "list") 
        return(F)
    if (!all(sapply(x, is.User.class))) 
        return(F)
    return(T)
}

#'User.list
#'@param x an object to form a 'User.list' from
#'@export
User.list <- function(x, ...) {
    UseMethod("User.list", x)
}

#'@method User.list default
User.list.default <- function(x, ...) {
    class(x) <- "User.list"
    if (!valid(x)) 
        stop()
    x
}

#'@method User.list character
User.list.character <- function(x, ...) {
    if (length(x) == 1 && file.exists(x)) 
        x <- scan(x, what = "character", sep = "\n")
    x <- lapply(x, as.User.class)
    NextMethod()
}

#'@method is User.list
is.User.list <- function(object, ...) inherits(object, "User.list")

#'@method as User.list
as.User.list <- function(object, ...) User.list(object)

#'@method format User.list
format.User.list <- function(x, basedn = NULL, ...) UseMethod("format.User.list", 
    basedn)

#'@method format.User.list default
format.User.list.default <- function(x, basedn = NULL, ...) paste(collapse = "\n", 
    sapply(x, format))

#'@method format.User.list basedn.class
format.User.list.basedn.class <- function(x, basedn = NULL, ...) {
    args <- lapply(x, format, basedn)
    args$sep <- "\n"
    do.call(paste, args)
}

#'@method print User.list
print.User.list <- function(x, ...) cat(format(x, ...), "\n")

UserGen <- function(usernames, domain, uidRange = c(1500, 65500), defaultGroup = NULL, 
    homePart = "/home", defaultShell = "/bin/bash") {
    if (!is.domain.class(domain)) 
        domain <- domain.class(domain)
    n <- length(usernames)
    pws <- paste(sep = "@", usernames, toupper(domain))
    uids <- sample(do.call(seq, as.list(uidRange)), n)
    if (missing(defaultGroup)) 
        gids <- uids else gids <- rep(defaultGroup, n)
    gec <- lapply(usernames, gecos)
    homes <- paste(sep = "/", homePart, usernames)
    shells <- rep(defaultShell, n)
    x <- cbind(usernames, pws, uids, gids, gec, homes, shells)
    User.list(lapply(seq(n), function(i) User.class(x[i, ])))
}

