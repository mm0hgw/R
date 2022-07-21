#' integer.precision.limit
#'@description A constant. The maximum double value the base system can handle
#'with integer precision.\cr 
#'\code{
#'max(.Machine$integer.max,
#'2^.Machine$double.digits-1
#')
#'/}
#'@export
integer.precision.limit <- 2^(.Machine$double.digits) - 1
