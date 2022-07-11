#'is.ultraCombo
#'@param x object to test
#'@export
is.ultraCombo <- function(x) {
    if (!(inherits(x, "ultraCombo")))
        return(FALSE)
    if (!is.valid.nk(x$n, x$k))
        return(FALSE)
    if (!is.valid.index(x$i, x$n, x$k))
        return(FALSE)
    return(TRUE)
}

#' is.valid.nk
#'@description Validator function for n/k values defining combinations.
#'Only combination definitions capable of yielding at least one combination
#'are considered valid in this test.
#'@param n an integer, n of combination
#'@param k an integer, k of combination
#'@examples
#'# length(n)==1
#'try(is.valid.nk(c(0,0),0))
#'# length(k)==1
#'try(is.valid.nk(0,c(0,0)))
#'# n%%1==0
#'try(is.valid.nk(0.5,0))
#'# k%%1==0
#'try(is.valid.nk(0,0.5))
#'# k<=n<.Machine$integer.max
#'try(is.valid.nk(c(0,.Machine$integer.max+1),1))
#'# 0<k<n
#'try(is.valid.nk(0,c(-1,1)))
#'# all
#'try(is.valid.nk(c(-1.5,.Machine$integer.max+1),c(-0.5,.Machine$integer.max+1e3)))
#'
#'# low limit of validity
#'is.valid.nk(0,0)
#'is.valid.nk(1,0)
#'# high limit of validity
#'is.valid.nk(.Machine$integer.max,.Machine$integer.max)
#'@export
is.valid.nk <- function(n, k) {
    # cat('is.valid.nk:',n,k,'\n')
    error_list <- vector()
    if (length(n) != 1) {
        error_list <- c(error_list, paste("n argument is invalid length", paste(n,
            collapse = ",")))
    }
    if (length(k) != 1) {
        error_list <- c(error_list, paste("k argument is invalid length", paste(k,
            collapse = ",")))
    }
    if (sum(n%%1 != 0) > 0) {

        error_list <- c(error_list, paste("non integer n argument:", paste(n[n%%1 !=
            0], collapse = ",")))
    }
    if (sum(k%%1 != 0) > 0) {
        error_list <- c(error_list, paste("non integer k argument:", paste(k[k%%1 !=
            0], collapse = ",")))
    }
    maxint <- .Machine$integer.max
    if (sum(c(n < k, n > maxint)) > 0) {
        error_list <- c(error_list, paste("n argument out of valid range:", n))
    }
    if (sum(c(k < 0, k > n)) > 0) {
        error_list <- c(error_list, paste("k argument out of valid range", k))
    }
    if (length(error_list) > 0) {
        stop(paste(error_list, collapse = ":"))
    } else {
        return(TRUE)
    }
}

#'is.valid.index
#'@param i A vector of indices
#'@inheritParams is.valid.nk
#'@examples
#'n<-20
#'k<-10
#'# non-integer input
#'try(is.valid.index(0.5,n,k))
#'# out of range input
#'try(is.valid.index(0,n,k))
#'try(is.valid.index(choose(n,k)+1,n,k))
#'# valid tests
#'# no argument
#'is.valid.index(vector(),n,k)
#'# single valid argument
#'is.valid.index(1,n,k)
#'# multiple valid aguments
#'is.valid.index(c(1,choose(n,k)),n,k)
#'@export
is.valid.index <- function(i, n, k) {
    # debugLine('is.valid.index',i,n,k)
    error_list <- vector()
    errors <- i[i%%1 != 0]
    if (length(errors) > 0) {
        error_list <- c(error_list, paste("non integer i input:", paste(errors, collapse = ":")))
    }
    errors <- c(i[(i < 1)], i[(i > superChoose(n, k))])
    if (length(errors) > 0) {
        error_list <- c(error_list, paste("i values out of range:", paste(errors,
            collapse = ":")))
    }
    if (length(error_list) > 0) {
        stop(paste(error_list, collapse = ":"))
    } else {
        return(sw_cond(i))
    }
}

#'is.valid.combination
#'@param x A vector of combination members
#'@param n an integer, n of the combination
#'@examples
#'n<-20
#'# non integer
#'try(is.valid.combination(0.5,n))
#'# values out of range
#'try(is.valid.combination(c(0,n+1),n))
#'# duplicate values
#'try(is.valid.combination(c(1,1),n))
#'# valid
#'# no argument
#'is.valid.combination(vector(),n)
#'# single argument
#'is.valid.combination(c(1,n),n)
#'# no argument
#'is.valid.combination(matrix(c(1,1,n,n),2),n)
#'@export
is.valid.combination <- function(x, n) {
    # debugLine('is.valid.combination',n,paste(x,collapse=','))
    if (length(dim(x)) > 1) {
        lapply(seq(nrow(x)), function(y) is.valid.combination(x[y, ], n))
        return(3)
    } else {
        error_list <- vector()
        errors <- x[x%%1 != 0]
        if (length(errors) > 0) {
            error_list <- c(error_list, paste("non integer x input:", paste(errors,
                collapse = ":")))
        }
        errors <- c(x[(x < 1)], x[(x > n)])
        if (length(errors) > 0) {
            error_list <- c(error_list, paste("x values out of range:", paste(errors,
                collapse = ":")))
        }
        errors <- x[duplicated(x)]
        if (length(errors) > 0) {
            error_list <- c(error_list, paste("duplicate x values", paste(errors,
                collapse = ":")))
        }
        if (length(error_list) > 0) {
            stop(paste(error_list, collapse = ":"))
        } else {
            if (length(x) != 0)
                return(2)
            return(1)
        }
    }
}

sw_cond <- function(x) {
    min(length(x) + 1, 3)
}
