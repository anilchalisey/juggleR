#' Remove trailing white spaces
#'
#' Remove trailing white spaces (or other character) from a string
#'
#' @param str character string to be trimmed
#' @param before a logical which indicates whether to remove the
#'  character from the beginning of the string
#' @param after a logical which indicates whether to remove the
#'  character from the end of the string
#' @param char the character to be removed
#'
#' @return
#' Returns a string from which the character specified has been removed.
#'   If the argument \code{char} is not specified, then this will be trailing
#'   white spaces.
#'
#' @export
#'
#' @examples
#' str <- "  this is a test   "
#' x <- removeTrail(str, before = TRUE)
#' x <- removeTrail(str, after = TRUE)
#' x <- removeTrail(str, before = TRUE, after = TRUE)
#' x <- removeTrail(str, before = TRUE, char = "  th")

removeTrail <- function(str, before = TRUE, after = TRUE, char = " ") {
  if (!is.character(str)) {
    warning("not a character() type")
    return(str)
  }
  ch <- substr(paste(char)[1], 1, 1)
  kk <- (length(str))
  if (kk < 1) return(str)
  for (cc in 1:kk) {
    if (isTRUE(before)) {
      while (substr(str[cc], 1, 1) == ch) {
        if (nchar(str[cc]) > 1) {
          str[cc] <- substr(str[cc], 2, nchar(str[cc]))
        } else {
          str[cc] <- gsub(ch, "", str[cc])
        }
      }
    }
    if (after) {
      while (substr(str[cc], nchar(str[cc]), nchar(str[cc])) == ch) {
        if (nchar(str[cc]) > 1) {
          str[cc] <- substr(str[cc], 1, nchar(str[cc]) - 1)
        } else {
          str[cc] <- gsub(ch, "", str[cc])
        }
      }
    }
  }
  return(str)
}
