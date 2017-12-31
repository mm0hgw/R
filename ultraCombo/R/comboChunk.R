#'comboChunk
#'@description Chunk combos. See bit::chunk
#'@param combo an 'ultraCombo' to chunk
#'@param ... arguments for bit::chunk
#'@export
comboChunk <- function(combo, ...) {
    UseMethod("comboChunk", combo)
}

#'comboChunk.ultraCombo
#'@param combo an 'ultraCombo' to chunk
#'@param ... arguments for bit::chunk
#'@importFrom bit chunk
#'@method comboChunk ultraCombo
#'@export
comboChunk.ultraCombo <- function(combo, ...) {
    lapply(bit::chunk(from = 1, to = combo$len, ...), function(ch) {
        combo$i <- combo$i[seq(ch[1], ch[2])]
        combo$len <- length(combo$i)
        return(combo)
    })
}

