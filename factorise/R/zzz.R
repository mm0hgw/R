# setup cache

primesEnv <- new.env()

resetCache()
setChunkSize(1e+06)

precisionLimit <- 2^.Machine$double.digits - 1
