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
    lapply(bit::chunk(combo$i, ...), function(i) {
        ultraCombo(i, combo$n, combo$k)
    })
}

