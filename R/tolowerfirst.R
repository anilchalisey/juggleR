#' Convert first character to lower or upper case
#'
#' Given a character vector, convert the first character to lower or upper case.
#'
#' \code{tolowerfirst} converts the first character in a string to lower case, and
#' \code{toupperfirst} converts the first character in a string to lower case.
#'
#' @param x A character vector
#'
#' @return The original character vector with the first character of every
#'   element converted to lower or upper case
#'
#' @examples
#' tolowerfirst("CamelCase")
#' tolowerfirst("ALLCAPS")
#' tolowerfirst(c("First", "_Second"))
#' toupperfirst("camelcase")
#' toupperfirst("allcaps")
#' toupperfirst(c("first", "_second"))
#'
#' @name firstcharacter
NULL

#' @export
#' @rdname firstcharacter

tolowerfirst <- function(x) {
  stopifnot(is.character(x))
  paste0(tolower(substring(x, 1, 1)), substring(x, 2))
}

#' @export
#' @rdname firstcharacter

toupperfirst <- function(x) {
  stopifnot(is.character(x))
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
