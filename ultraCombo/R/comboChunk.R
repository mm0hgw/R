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
#'@importFrom getLapply chunk
#'@method comboChunk ultraCombo
#'@export
comboChunk.ultraCombo <- function(combo, ...) {
    lapply(getLapply::chunk(length(combo)), function(seqArgs) {
        ultraCombo(combo$i[do.call(seq,seqArgs)], combo$n, combo$k)
    })
}

