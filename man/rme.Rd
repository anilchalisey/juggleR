% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rme.R
\name{rme}
\alias{rme}
\title{Remove Objects Except}
\usage{
rme(except = NULL)
}
\arguments{
\item{except}{Character vector of the names of the objects to keep}
}
\value{
None.
}
\description{
\code{rme} removes all objects in the global environemnt except the ones specified
}
\details{
Acts as a wrapper around \code{rm} to keep the specified objects.
}
