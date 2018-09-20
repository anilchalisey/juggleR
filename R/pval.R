#' @title p-values formatter and significance stars
#'
#' @description Formatter for p-values, adding a symbol "<" for very small p-values and,
#' optionally, significance stars
#'
#' @param x A numeric vector of p-values
#' @param accuracy Number to round to [Default = 0.001]
#' @param stars If \code{TRUE} then add significance stars [Default = FALSE]
#' @param three Number indicating level below which to display three stars '***' [Default = 0.001]
#' @param two Number indicating level below which to display two stars '**' [Default = 0.01]
#' @param one Number indicating level below which to display one star '*' [Default = 0.05]
#'
#' @export
#'
#' @return A character vector
#'
#' @examples
#' p <- c(.50, 0.12, .09, .045, .011, .009, .00002, NA)
#' pval(p)
#' pval(p, accuracy = .01)
#' pval(p, stars = TRUE)
pval <- function(x, accuracy = .001, stars = FALSE,
                 three = 0.001, two = 0.01, one = 0.05) {
  res <- number(x, accuracy = accuracy, big.mark = "", decimal.mark = ".")
  digits <- -floor(log10(accuracy))
  res[x < 10 ^ -digits] <- paste0("<", number(10 ^ -digits, accuracy = accuracy))
  if (stars)
    res <- paste(res, signifStars(x, three, two, one))
  res
}

#' @rdname pval
#' @export signifStars
#' @examples
#' signifStars(p)
#' signifStars(p, one = .15)
signifStars <- function(x, three = 0.001, two = 0.01, one = 0.05) {
  res <- rep_len("", length.out = length(x))
  if (!is.null(one))
    res[x <= one] <- "*"
  if (!is.null(two))
    res[x <= two] <- "**"
  if (!is.null(three))
    res[x <= three] <- "***"
  res
}
