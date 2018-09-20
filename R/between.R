#' @title Between
#'
#' @description Determine if a numeric value is within a range
#'
#' @details \code{x \%btwn\% range} is equivalent to \code{beween(x, range, inclusive = TRUE)}
#'
#' @param x Numeric value or vector to check
#' @param range Numeric vector of two elements, defining the range to check
#' @param inclusive Logical indicating whether or not to include the lower and upper
#' values of the range [Default = TRUE]
#'
#' @return a logical vector of the same length as \code{x}.  For every element in \code{x} returns
#' \code{TRUE} if the value lies between the range and \code{FALSE} if not.
#'
#' @examples
#' 5 %btwn% c(1, 10)
#' c(5, 20) %btwn% c(5, 10)
#' between(5, c(5, 10))
#' between(5, c(5, 10), inclusive = FALSE)
#'
#' @name between
NULL

#' @export
#' @rdname between
"%btwn%" <- function(x, range) {
  between(x, range, inclusive = TRUE)
}

#' @export
#' @rdname between
between <- function(x, range, inclusive = TRUE) {
  stopifnot(is.numeric(x), is.numeric(range), length(range) == 2, is.logical(inclusive))
  range <- sort(range)
  if (inclusive) {
    x >= range[1] & x <= range[2]
  } else {
    x > range[1] & x < range[2]
  }
}
