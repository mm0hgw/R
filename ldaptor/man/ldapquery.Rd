% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RFC2253.query.R
\name{ldapquery}
\alias{ldapquery}
\title{ldapquery}
\usage{
ldapquery(x)
}
\arguments{
\item{x}{a 'list' with the elements : \cr 
pkey a 'ldapkv' the primary key for the query\cr
basedn a valid 'basedn.class' the base dn for the query\cr
skeylist a 'ldapkvlist' the secondary keys for the query\cr
kvlist a 'ldapkvlist' the content of the query}
}
\description{
a container for a valid LDAP query
}
