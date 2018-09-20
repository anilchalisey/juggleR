#' @title Round a number, preserving extra 0s
#'
#' @description Round a number, preserving extra 0s.
#'
#' @param x Vector of numbers to round.
#' @param digits Number of digits past the decimal point to keep.
#'
#' @details
#' Uses \code{\link[base]{sprintf}} to round a number, keeping extra 0s.
#'
#' @export
#'
#' @return
#' A character vector of rounded numbers.
#'
#' @examples
#' betterRound(51.01, 3)
#' betterRound(0.199, 2)

betterRound <- function(x, digits = 2L) {
  digits <- as.integer(digits)
  if (digits < 1) {
    stop("Digits must be a positive integer value.")
  }
  if(length(digits) > 1) {
    digits <- digits[1]
    warning("Digits should be a single integer; using only the first value")
  }

  tmp <- sprintf(paste("%.", digits, "f", sep = ""), x)

  # deal with "-0.00" case
  zero <- paste0("0.", paste(rep("0", digits), collapse = ""))

  tmp[tmp == paste0("-", zero)] <- zero
  tmp
}


