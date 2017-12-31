#' superChoose 
#'@description 
#' Choose values calculated with integer accuracy.
#'@inheritParams gmp::chooseZ
#'@examples
#'# regular base::choose() output
#'outer(seq(11),seq(11),FUN=superChoose)
#'n<-1
#'# step up to below system limit
#'while(hashChoose(n+1,(n+1)%/%2)<integer.precision.limit){
#'n<-n+1
#'}
#'k<-n%/%2
#'# this is accurate
#'hashChoose(n,k)
#'hashChoose(n,k-1)
#'# the above should exactly sum to the below
#'hashChoose(n+1,k)
#'class(superChoose(n,k))
#'# throws an error without 'gmp' installed
#'try(superChoose(n+1,k)-hashChoose(n+1,k))
#'if(require(gmp)){
#'print(out<-superChoose(n+1,k))
#'print(class(out))
#'print(paste('n',n,'k',k))
#'print(outer(seq(5)-3+n,seq(3)-2+k,FUN=superChoose))
#'print(outer(seq(11)-6+n,seq(11)-6+k,FUN=superChoose) -
#'outer(seq(11)-6+n,seq(11)-6+k,FUN=hashChoose))
#'}
#'@importFrom methods as
#'@export
superChoose <- function(n, k) {
    is.valid.superChoose.nk(n, k)
    if (requireNamespace("gmp")) {
        out <- gmp::chooseZ(n, k)
        if (all(out < integer.precision.limit)) {
            as(out, class(integer.precision.limit))
        } else {
            out
        }
    } else {
        out <- hashChoose(n, k)
        if (any((integer.precision.limit > out))) {
            stop("Computation out of integer precision range. Install 'gmp' to extend precision range.")
        }
        return(out)
    }
}

is.valid.superChoose.nk <- function(n, k) {
    z <- c(n, k)
    errors <- z[z < 0]
    if (length(errors) > 0) {
        stop("negative input")
    }
    errors <- z[z%%1 != 0]
    if (length(errors) > 0) {
        stop("noninteger input")
    }
    return(TRUE)
}

chooseEnv <- new.env()

#'hashChoose
#'@description
#'Used for better integer accuracy over non 
#' negative integer input than base::choose(n,k)
#'@inheritParams gmp::chooseZ
#'@export
hashChoose <- function(n, k) {
    if (length(n) > 1 || length(k) > 1) {
        nk <- cbind(n, k)
        do.call(c, lapply(seq(nrow(nk)), function(x) hashChoose(nk[x, 1], nk[x, 2])))
    } else {
        if (k > n) 
            return(0)
        if (2 * k > n) 
            k <- n - k
        if (k == 0) 
            return(1)
        if (k == 1) 
            return(n)
        name <- paste(n, k, sep = "C")
        if (exists(name, envir = chooseEnv)) {
            out <- get(name, envir = chooseEnv)
            out
        } else {
            out <- hashChoose(n - 1, k - 1) + hashChoose(n - 1, k)
            assign(name, out, envir = chooseEnv)
            out
        }
    }
}
