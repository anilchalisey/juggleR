#' @title 'Characterize' data.frame
#'
#' @description Convert factor columns in a data.frame to characters.
#'
#' @details Given a data.frame, convert factor columns to characters.  By default,
#' all factor columns are converted, but the user can specify which columns
#' to convert or to not convert.
#'
#' @note If any of the names in \code{only} or \code{ignore} are not valid
#'   columns, an error is raised.
#' @note Only one of \code{only} or \code{ignore} can be used in a single call.
#'
#' @param df A data.frame.
#' @param only A vector of column names. Only convert these columns to characters.
#' @param ignore A vector of column names. Do not convert these columns.
#' @return A data.frame with all factor columns (or only a subset of them
#'   if the parameters were set) converted to characters.
#'
#' @export
#'
#' @return a data.frame with factor columns converted to characters.
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3], c = LETTERS[1:3],
#'                  stringsAsFactors = TRUE)
#' str(df)
#' str(dfCharacterize(df))
#' str(dfCharacterize(df, only = "b"))
#' str(dfCharacterize(df, ignore = "b"))
#' str(dfCharacterize(df, ignore = c("b", "c")))
#' str(dfCharacterize(df, only = c("a")))
#'
#' # The following examples result in errors
#' \dontrun{str(dfCharacterize(df, only = c("z")))}
#' \dontrun{str(dfCharacterize(df, only = "b", ignore = "c"))}

dfCharacterize <- function(df, only = c(), ignore = c()) {
  # Check parameters
  stopifnot(
    is.data.frame(df),
    length(only) == 0 || length(ignore) == 0,
    all(c(only, ignore) %in% colnames(df))
  )

  # Determine which columns to change
  colsConvert <- rep(TRUE, ncol(df))
  if (length(only) > 0) {
    colsConvert <- colnames(df) %in% only
  }
  if (length(ignore) > 0) {
    colsConvert <- colnames(df) %nin% ignore
  }
  colsConvert <- colsConvert & sapply(df, is.factor)

  # Convert specific columns to characters
  df[colsConvert] <- lapply(df[colsConvert], as.character)
  df
}
