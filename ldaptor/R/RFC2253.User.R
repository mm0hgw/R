

UserClasses <- c("character", "character", "integer", "integer", "gecos", "character", 
    "character")

#'@method valid User.class
valid.User.class <- function(x, class1 = "User.class") {
    if (typeof(x) != "list") 
        return(F)
    if (length(x) != 7) 
        return(F)
    if (any(sapply(x, length) != 1)) 
        return(F)
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
        x <- lapply(seq(7), function(i) paste(sep = "", "as.", UserClasses[i])(list(x[[i]])))
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
is.User.class <- function(object, ...) inherits("User.class", object)

#'@method as.User.class default
as.User.class <- function(object, ...) User.class(object)

#'@method format User.class
format.User.class <- function(x, ...) paste(collapse = ":", sapply(x, format))

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
    if (!valid(gecos)) 
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
is.gecos <- function(object, ...) inherits("gecos", object)

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
User.list <- function(x = "/etc/passwd", ...) {
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
User.list.character <- function(x = "/etc/passwd", ...) {
    if (length(x) == 1 && file.exists(x)) 
        x <- scan(x, what = "character", sep = "\n")
    x <- lapply(x, as.User.class)
    NextMethod()
}

#'@method is User.list
is.User.list <- function(object, ...) inherits("User.list", object)

#'@method as.User.list default
as.User.list.default <- function(object, ...) User.list(object)

#'@method format User.list
format.User.list <- function(x, ...) paste(collapse = "\n", sapply(x, format))

#'@method print User.list
print.User.list <- function(x, ...) cat(format(x, ...), "\n")
