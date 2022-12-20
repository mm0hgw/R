#'multiUnion-package
#'@aliases NULL
"_PACKAGE"

#'multiUnion
#'@description A union function 
#' which takes multiple arguments.
#'@param ... vectors of values
#'@export
multiUnion <- function(...) {
    arg <- list(...)
    if (length(arg) == 0)
        return(vector())
    while (length(arg) > 1) {
        temp <- lapply(seq(length(arg)%/%2), function(x) {
            union(arg[[2 * x - 1]], arg[[2 * x]])
        })
        if (length(arg)%%2 == 1) {
            temp <- append(temp, arg[length(arg)])
        }
        arg <- temp
    }
    arg[[1]]
}

#'multiIntersect
#'@param ... vectors of values
#'@export
multiIntersect <- function(...) {
    l1 <- list(...)
    u <- do.call(multiUnion, l1)
    l2 <- lapply(l1, setdiff, x = u)
    l3 <- do.call(multiUnion, l2)
    setdiff(u, l3)
}
