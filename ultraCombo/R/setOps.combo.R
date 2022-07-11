#'setdiff.combo
#'@examples
#'a<-ultraCombo(seq(4),4,2)
#'b<-ultraCombo(seq(3)+2,4,2)
#'a$i
#'b$i
#'stopifnot(all(c(1,2)==setdiff.combo(a,b)$i))
#'stopifnot(all(c(1,4)==setdiff.combo(a,seq(2)+1)$i))
#'stopifnot(all(c(1,2,6)==setdiff.combo(seq(6),b)$i))
#'@param a,b one ultraCombo and one ultraCombo or vector
#'@export
setdiff.combo <- function(a, b) {
    l <- validateInput(a, b)
    i <- setdiff(l[[1]]$i, l[[2]]$i)
    n <- l[[1]]$n
    k <- l[[1]]$k
    return(ultraCombo(i, n, k))
}

#'intersect.combo
#'@examples
#'a<-ultraCombo(seq(4),4,2)
#'b<-ultraCombo(seq(3)+2,4,2)
#'a$i
#'b$i
#'stopifnot(all(c(3,4)==intersect.combo(a,b)$i))
#'stopifnot(all(c(2,3)==intersect.combo(a,seq(2)+1)$i))
#'print(intersect.combo(seq(3)+3,b)$i)
#'stopifnot(all(c(4,5)==intersect.combo(seq(3)+3,b)$i))
#'stopifnot(all(c(4)==intersect.combo(a,b,4)$i))
#'@inheritParams union.combo
#'@export
intersect.combo <- function(...) {
    l1 <- validateInput(...)
    l2 <- lapply(l1, "$", "i")
    i <- do.call(multiIntersect, l2)
    n <- l1[[1]]$n
    k <- l1[[1]]$k
    ultraCombo(i, n, k)
}

#'invert.combo
#'@param a an ultraCombo object
#'@examples
#'n<-4
#'k<-2
#'a<-ultraCombo(vector(),n,k)
#'b<-invert.combo(a)
#'print(b)
#'print(b$i)
#'stopifnot(all(b$i==seq(6)))
#'@export
invert.combo <- function(a) {
    setdiff.combo(seq(choose(a$n, a$k)), a)
}

#'union.combo
#'@examples
#'a<-ultraCombo(seq(4),4,2)
#'b<-ultraCombo(seq(3)+2,4,2)
#'a$i
#'b$i
#'stopifnot(all(seq(5)==union.combo(a,b)$i))
#'print(seq(4))
#'print(a$i)
#'print(seq(2)+1)
#'print(union.combo(a,seq(2)+1)$i)
#'stopifnot(all(seq(4)==union.combo(a,seq(2)+1)$i))
#'stopifnot(all(seq(4)+2==union.combo(b,6)$i))
#'stopifnot(all(seq(6)==union.combo(a,b,6)$i))
#'@param ... at least one ultraCombo object, and any amount of ultraCombos or vectors
#'@export
union.combo <- function(...) {
    l <- validateInput(...)
    i <- do.call(multiUnion, lapply(l, function(x) x$i))
    n <- l[[1]]$n
    k <- l[[1]]$k
    return(ultraCombo(i, n, k))
}

validateInput <- function(...) {
    l <- list(...)
    m <- sapply(l, function(i) {
        "ultraCombo" %in% class(i)
    })
    lc <- l[m, drop = FALSE]
    if (length(lc) == 0) {
        stop("no combo in args")
    }
    n <- sapply(lc, function(x) x$n)
    k <- sapply(lc, function(x) x$k)
    if (sum(duplicated(n)) != length(n) - 1 || sum(duplicated(k)) != length(k) -
        1) {
        stop("input combos disagree on n,k parameters")
    }
    n <- n[1]
    k <- k[1]
    lv <- l[!m, drop = FALSE]
    if (length(lv) != 0) {
        l[!m] <- lapply(lv, ultraCombo, n, k)
    }
    l
}
