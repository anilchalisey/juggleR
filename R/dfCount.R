#' @title Count number of rows per group
#'
#' @description Count how many times each distinct value of a data.frame column is
#' observed.
#'
#' @details
#' \code{dfCount(x, "y")} is similar in functionality to \code{table(x$y)}, but
#' performs better on large datasets.
#' There are two main differences between \code{dfCount} and \code{table}:
#'   1. \code{dfCount} returns a \code{data.frame} instead of \code{table}
#'      object
#'   2. \code{dfCount} includes a row for number of NA observations, whereas
#'      \code{table} does not by default
#'
#' @note The \code{dplyr} package is required for this function.
#'
#' @section Performance:
#' This function performs much faster than its equivalent \code{table} call on
#' large datasets, even though the \code{table} function does not sort the
#' results. The main speed boost is due to the fact that `dplyr` is used.
#'
#' @param df A data.frame.
#' @param col The column to count.
#' @param sort Whether or not to sort the resulting total column.
#' @param name The name of the total column.
#'
#' @return A data.frame with two columns: The first column is the distinct
#'   values of the given variable, the second column shows the total number of
#'   rows with that value.
#'
#' @export
#'
#' @importFrom dplyr group_by_ summarise_ ungroup arrange_ desc vars funs
#' @importFrom lazyeval interp
#' @importFrom magrittr %>% %<>%
#'
#' @examples
#' flights <- nycflights13::flights
#' dfCount(flights, "dest")
#' dfCount(flights, "dest", sort = FALSE)
#' dfCount(flights, "dest", name = "flights")
#'
#' dfCount(infert, "education")
#' dfCount(infert, "education", sort = FALSE)
#' data.frame(table(infert$education))

dfCount <- function(df, col, sort = TRUE, name = "total") {

	# Check parameters
	stopifnot(
		is.data.frame(df),
		length(col) == 1,
		col %in% colnames(df),
		is.bool(sort)
	)

	# Count the number of observations per group
	df %<>%
		dplyr::group_by_(col) %>%
		dplyr::summarise_("total" = ~n()) %>%
		dplyr::ungroup()

	# Sort (most observations near the top)
	if (sort) {
		df %<>%
			dplyr::arrange_(lazyeval::interp(~dplyr::desc(var),
																			 var = as.name("total"))) %>%
	    dplyr::mutate_at(dplyr::vars(col), dplyr::funs(factor))
	}
	df %<>% stats::setNames(c(col, name))
	df
}
