#'forkBombGen
#'
#'A function generator to simplify parallelisation.
#'@param FUN a single argument function to parallelise.
#'@param COLLATEFUN a function used to collate results. Default is list()
#'@return When run with list() as the COLLATEFUN() parameter, a list per
#'thread run, of lists per argument run through FUN().
#'@export
forkBombGen <- function(FUN, COLLATEFUN = list) {
    function(x) {
        LAPPLYFUN = getLapply()
        chunks <- chunk(length(x))
        # print(chunks)
        do.call(COLLATEFUN, LAPPLYFUN(chunks, function(seqArgs) {
            do.call(COLLATEFUN, lapply(do.call(seq, seqArgs), function(z) {
                FUN(x[z])
            }))
        }))
    }
}
