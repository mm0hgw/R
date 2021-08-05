#'dataCombo
#'@description A method to wrap a data object with an ultraCombo.
#'@param combo an 'ultraCombo'
#'@param dataObj a data object
#'@param FUN a 'function' used to process data after slicing. Default is invisible()
#'@export
dataCombo <- function(combo, dataObj) {
    stopifnot(is.ultraCombo(combo))

    out <- combo
    combnGen <- combnGG(combo$n, combo$k)
    if (is.null(dim(dataObj))) {
        wrappedFunGen <- function(FUN) {
            function(iX) {
                FUN(dataObj[combnGen(iX)])
            }
        }
    } else {
        wrappedFunGen <- function(FUN) {
            function(iX) {
                FUN(dataObj[combnGen(iX), TRUE])
            }
        }
    }
    out$dGen <- function(FUN = return, COLLATEFUN = list) {
        stopifnot(is.function(FUN) || is.primitive(FUN))
        WRAPPEDFUN <- wrappedFunGen(FUN)
        FORKBOMBFUN <- forkBombGen(WRAPPEDFUN, COLLATEFUN)
        FORKBOMBFUN(combo$i)
    }
    out$dataObj <- dataObj
    class(out) <- c("dataCombo", class(out))
    out
}

#'print.dataCombo
#'@method print dataCombo
#'@export
print.dataCombo <- function(x, ...) {
    cat(" -=* dataCombo object *=-\n")
    summary(x$dataObj)
    cat("   ----====* & *====----\n")
    NextMethod()
}
