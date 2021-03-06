% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringSearch.R
\name{stringSearch}
\alias{stringSearch}
\alias{findString}
\alias{replaceString}
\title{Search and replace strings in a file}
\usage{
findString(string = NULL, file = NULL)

replaceString(string = NULL, replacement = NULL, file = NULL)
}
\arguments{
\item{string}{String to search for or replace}

\item{file}{The file to search}

\item{replacement}{Replacement string}
}
\value{
\code{findString} returns the lines in the file containing the string, and
\code{replaceString} invisibly replaces the string in place
}
\description{
Search for and replace strings in a file
}
\details{
\code{findString} searches for a string in a file, and \code{replaceString}
  replaces a string in a file.
}
