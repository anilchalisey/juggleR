#' @title Number formatters
#'
#' @description
#' \code{number} is a generic formatter for numeric values.
#' \code{addCommas} is a shortcut for comma as separator for thousands, point for decimal.
#' \code{addSpaces} is a shortcut for space as separator for thousands, point for decimal.
#' \code{percent} a shortcut for percentages (value are multiplied by 100 and a % symbol is added).
#' \code{compPercent} returns the complement of 1, i.e. \code{percent(1 - x)}.
#'
#' @return a formatted character vector
#'
#' @param x a numeric vector to format
#' @param accuracy number to round to, \code{NULL} for automatic guess.
#' [10 wll round to nearest 10, 1 will round to nearest integer, 0.1 to nearest 0.1, etc.]
#' @param multiplier number to multiply by (e.g. for computing percentages or thousands)
#' @param prefix,suffix Symbols to display before and after value
#' @param big.mark character used between every 3 digits to separate thousands
#' @param decimal.mark the character to be used to indicate the numeric decimal point
#'
#' @rdname number
#'
#' @importFrom scales zero_range
#'
#' @export
#'
#' @examples
#' v <- c(12.3, 4, 12345.789, 0.0002)
#' number(v)
#' addCommas(v, accuracy = 0.01)
#' p <- runif(10)
#' percent(p)
#' compPercent(p)

number <- function(x, accuracy = 1, multiplier = 1,
                   prefix = "", suffix = "",
                   big.mark = " ", decimal.mark = ".") {

  if (length(x) == 0) return(character())

  if (is.null(accuracy)) {
    x <- round_any(x, precision(x) / multiplier)
    nsmall <- -floor(log10(precision(x)))
  } else {
    x <- round_any(x, accuracy / multiplier)
    nsmall <- -floor(log10(accuracy))
  }
  nsmall <- min(max(nsmall, 0), 20)
  paste0(prefix,
         format(multiplier * x, big.mark = big.mark, decimal.mark = decimal.mark,
                scientific = FALSE, trim = TRUE, nsmall = nsmall),
         suffix)
}

#' @export
#' @rdname number
addCommas <- function(x, accuracy = 1) {
  number(x, accuracy = accuracy, multiplier = 1, prefix = "",
         suffix = "", big.mark = ",", decimal.mark = ".")
}

#' @export
#' @rdname number
addSpaces <- function(x, accuracy = 1) {
  number(x, accuracy = accuracy, multiplier = 1, prefix = "",
         suffix = "", big.mark = " ", decimal.mark = ".")
}

#' @export
#' @rdname number
percent <- function(x, accuracy = 1) {
  number(x, accuracy = accuracy, multiplier = 100, prefix = "",
         suffix = "%", big.mark = ",", decimal.mark = ".")
}

#' @export
#' @rdname number
compPercent <- function(x, accuracy = 1) {
  number(1 - x, accuracy = accuracy, multiplier = 100, prefix = "",
         suffix = "%", big.mark = ",", decimal.mark = ".")
}


round_any <- function(x, accuracy) {
  round(x / accuracy) * accuracy
}

precision <- function(x) {
  rng <- range(x, na.rm = TRUE)
  span <- if (scales::zero_range(rng)) {
    abs(rng[1])
  } else {
    diff(rng)
  }
  if (span == 0) {
    return(1)
  } else {
    return(10 ^ floor(log10(span)))
  }
}
