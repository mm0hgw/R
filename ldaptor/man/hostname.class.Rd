% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RFC1123.host.R
\name{hostname.class}
\alias{hostname.class}
\title{hostname.class}
\usage{
hostname.class(host)
}
\arguments{
\item{host}{a 'character' identifier}
}
\description{
a container for a valid hostname
}
\examples{
hexchars <-'0123456789ABCDEF'
stopifnot(valid.hostname.class(vector())==FALSE)
stopifnot(valid.hostname.class('')==FALSE)
stopifnot(valid.hostname.class(paste(collapse='',rep(4,hexchars)))==FALSE)
stopifnot(valid.hostname.class(':')==FALSE)
stopifnot(valid.hostname.class(paste(collapse='',c(letters,LETTERS,'-')))==TRUE)
}
