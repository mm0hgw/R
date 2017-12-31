# RFC2253 special characters c(',', '+', '\'', '\\', '<', '>', ';')

#' RFC2253Character
#' @description a container RFC2253 character data 
#' @param string a 'character' string
#'@export
RFC2253Character <- function(string) {
    stopifnot(valid.RFC2253Character(string))
    class(string) <- "RFC2253Character"
    string
}

#'@method is RFC2253Character
is.RFC2253Character <- function(x) {
    inherits(x, "RFC2253Character")
}

#'@method valid RFC2253Character
valid.RFC2253Character <- function(x) {
    x <- as.character(x)
    if (any(gsub(RFC2253Regex, "", x) != "")) 
        stop(x)
    return(TRUE)
}

#'@method format RFC2253Character
format.RFC2253Character <- function(x, ...) {
    x
}

#'@method print RFC2253Character
print.RFC2253Character <- function(x, ...) {
    cat(format(x, ...), "\n")
}
