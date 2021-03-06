% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orderedFactor.R
\name{orderedFactor}
\alias{orderedFactor}
\title{Preserve factor order}
\usage{
orderedFactor(x = character(), ...)
}
\arguments{
\item{x}{A vector of data, usually taking a small number of distinct values.}

\item{...}{Other arguments passed on to \code{base::factor()}.}
}
\value{
A factor.
}
\description{
\code{orderedFactor()} is a convenience wrapper for
  \code{factor()} that orders the levels as they appear in the data if the
  \code{levels} argument is not specified.  This is valuable for making
  plots with ggplot2, where the axis labels are usually arranged in
  alphabetical order rather than the order in which they appear.
}
\examples{
orderedFactor(3:1)
orderedFactor(9:12, exclude=11)

}
