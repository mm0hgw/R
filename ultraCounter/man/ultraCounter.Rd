% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ultraCounter.R
\name{ultraCounter}
\alias{ultraCounter}
\title{ultraCounter}
\usage{
ultraCounter(...)
}
\arguments{
\item{...}{vectors containing categorical variables}
}
\value{
The ultraCounter object has features:
out$elems stores the original arguments,
out$colvals stores the value change by a +1 change in each column,
out$len caches the overall length, 
out$index(x) maps an integer in the index range to a corresponding list of parameters,
out$revIndex(...) maps a parameter set to an integer in the index range.
out$sample(k=1e3) select a uniformly random sample of k points.
}
\description{
Define a multidimensional parameters space, according to the sequence of vectors supplied as arguments.
These are treated as containing categorical variables.
}
