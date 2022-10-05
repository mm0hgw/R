knockoutRounds <- function(x) ceiling(log(length(x), base = 2))

firstRoundSize <- function(x) 2^knockoutRounds(x)

byes <- function(x) firstRoundSize(x) - length(x)

nHiByes <- function(x) byes(x)%/%2
hiByes <- function(x) if ((z <- nHiByes(x)) > 0) lapply(seq(z), function(y) c(x[y],
    "*bye*")) else list()

nLoByes <- function(x) ceiling(byes(x)/2)
loByes <- function(x) if ((z <- nLoByes(x)) > 0) lapply(seq(to = length(x), length.out = z),
    function(y) c(x[y], "*bye*")) else list()

firstRoundMatches <- function(x) {
    x <- as.character(x)
    matches <- firstRoundSize(x) - byes(x)
    matchCompetitors <- x[seq(from = 1 + nHiByes(x), to = length(x) - nLoByes(x))]
    lapply(seq(length(matchCompetitors)/2), function(y) matchCompetitors[c(y, length(matchCompetitors) -
        y + 1)])
}

firstRound <- function(x) c(hiByes(x), firstRoundMatches(x), loByes(x))
