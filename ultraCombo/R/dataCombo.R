#'dataCombo
#'@description A method to wrap a data object with an ultraCombo.
#'@param combo an 'ultraCombo'
#'@param dataObj a data object
#'@param FUN a 'function' used to process data after slicing. Default is invisible()
#'@param ... extra args for '['
#'@export
dataCombo <- function(combo, dataObj, FUN = return) {
    stopifnot(is.ultraCombo(combo))
    stopifnot(is.function(FUN) || is.primitive(FUN))
    
    out <- combo
    if (is.null(dim(dataObj))) {
        out$dGen <- function(i) {
            stopifnot(length(i) == 1)
            FUN(dataObj[combo$Gen(i)])
        }
    } else {
        out$dGen <- function(i) {
            stopifnot(length(i) == 1)
            FUN(dataObj[combo$Gen(i), TRUE])
        }
    }
    out$dataObj <- dataObj
    class(out) <- c("dataCombo", class(out))
    out
}
