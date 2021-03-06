% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RFC1123.domain.R
\name{domain.class}
\alias{domain.class}
\title{domain.class}
\usage{
domain.class(x)
}
\arguments{
\item{domain}{a 'character' identifier}
}
\description{
a container for a valid domaim name
}
\examples{
hexchars <-'0123456789ABCDEF'
library(valid)
ls()
stopifnot(valid(vector(),'domain.class')==FALSE)
stopifnot(valid('','domain.class')==FALSE)
stopifnot(valid(paste(collapse='',rep(16,hexchars)),'domain.class')==FALSE)
stopifnot(valid(':','domain.class')==FALSE)
stopifnot(valid(paste(collapse='',c(letters,,'.',LETTERS,'-')),'domain.class')==TRUE)
}
