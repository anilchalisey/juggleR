% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concatenate.R
\name{concatenate}
\alias{concatenate}
\alias{\%p\%}
\title{Concatenate operator}
\usage{
x \%p\% y

concatenate(x, y)
}
\arguments{
\item{x}{a first vector to paste}

\item{y}{a second vector to paste}
}
\description{
The \code{\%p\%} operator is a shortcut for \code{\link[base]{paste0}}.
}
\examples{
"abc" \%p\% "def"
"v_" \%p\% 1:3
c("A", "B", "C") \%p\% 1:3

}
