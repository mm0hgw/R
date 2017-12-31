#'@title Combination Generator Generator
#'@name combnGG
#'@author Dale Potter, \email{dale@@piratepress.org}
#'@description Creates a generator function that transforms
#'reference integer vectors into combination matrices.
#'
#'@inheritParams is.valid.nk
#'@return a 'function (x,.combine=rbind)' that takes
#'reference integers in the range 1:choose(n,k) and returns
#'the requested combinations bound as specified.
#'
#'@examples
#'#define combination
#'n<-8
#'k<-5
#'combnGen<-combnGG(n,k)
#'# generate single combinations
#'stopifnot(0==sum(combnGen(1)!=seq(k)))
#'stopifnot(0==sum(combnGen(2)!=c(seq(k-1),k+1)))
#'stopifnot(0==sum(combnGen(choose(n,k))!=seq(to=n,length.out=k)))
#'#define larger combination
#'n2<-choose(n,k)
#'k2<-9
#'combnGen2<-combnGG(n2,k2)
#'stopifnot(0==sum(combnGen2(1)!=seq(k2)))
#'stopifnot(0==sum(combnGen2(2)!=c(seq(k2-1),k2+1)))
#'stopifnot(0==sum(combnGen2(choose(n2,k2))!=seq(to=n2,length.out=k2)))
#'#generate random index
#'index <- ceiling(runif(1)*choose(n2,k2))
#'#use index to look up indices
#'indices <- combnGen2(index)
#'# generate result
#'combnGen(indices)
#'@useDynLib ultraCombo
#'@export
combnGG <- function(n, k) {
    debugCat("combnGG", n, k)
    p <- getProfile(n, k)
    if (k == 1) {
        combnGenElem <- function(index) {
            return(index)
        }
    }
    if (k == n) {
        combnGenElem <- function(index) {
            return(seq(n))
        }
    }
    if (k != 1 && k != n) {
        if (p$indexType != "bigz") {
            combnGenElem <- combnGenElemGenC(p)
        } else {
            combnGenElem <- combnGenElemGenR(p)
        }
    }
    combnGen <- function(index, .combine = rbind) {
        debugCat("combnGen", n, k)
        switch(is.valid.index(index, n, k), vector(), combnGenElem(index), do.call(.combine, 
            lapply(index, combnGenElem)))
    }
    return(combnGen)
}

#'@title Combination Generator
#'@name combnG
#'@author Dale Potter, \email{dale@@piratepress.org}
#'@description Generates combinations from a
#'reference integer vector. combnG(x,n,k)
#'is equivalent to combn(n,k)[,x] but does not
#'generate the rest of the table.\cr
#'@inheritParams is.valid.index
#'@inheritParams is.valid.nk
#'@param .combine the function with which the requested combinations will be bound.
#'@return a 'vector' or 'matrix'
#'the requested combinations bound as specified.
#'@examples
#'n<-20
#'k<-10
#'stopifnot(0==sum(combnG(1,n,k)!=seq(k)))
#'stopifnot(0==sum(combnG(2,n,k)!=c(seq(k-1),k+1)))
#'stopifnot(0==sum(combnG(choose(n,k),n,k)!=seq(to=n,length.out=k)))
#'combnG(ceiling(runif(10)*choose(n,k)),n,k)
#'
#'if(ultraCombo:::debugFlag==FALSE){
#'stopifnot(sum(combn(n,k)!=combnG(seq(choose(n,k)),n,k,cbind))==0) 
#'}
#'
#'@export
combnG <- function(i, n, k, .combine = rbind) {
    combnGen <- combnGG(n, k)
    combnGen(i, .combine)
}
