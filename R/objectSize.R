#' @title Object sizes
#'
#' @description Calculate the sizes of objects in the workspace.
#'
#' @param decreasing If \code{TRUE}, sort the objects from largest to smallest.
#' @param head If \code{TRUE}, only show the top \code{n} objects.
#' @param n Integer specifying the number of values to show if \code{head = TRUE}.
#'
#' @export
#'
#' @return A data.frame with the name, class, and size of each object. If the object
#' is a data.frame or matrix, it will also specify its dimensions.
#'
#' @examples
#' print(output <- objectSize())
#'
objectSize <- function (decreasing = TRUE,
                        head = FALSE, n = 10) {

  napply <- function(names, fn) {
    sapply(names, function(x)
      fn(get(x, pos = 1)))
  }

  if (length(ls(pos = 1)) == 0) return(message("There are no objects in the global environment"))

  names <- ls(pos = 1)
  obj.class <- napply(names, function(x) {
    as.character(class(x))[1]
  })

  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x),  units = "auto")
  })

  obj.size <- napply(names, utils::object.size)
  obj.dim <- t(napply(names, function(x) {
    as.numeric(dim(x))[1:2]
  }))

  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")

  out <- out[order(out[["Size"]], decreasing = decreasing), ]

  if (head) {
    out <- head(out, n)
  }

  return(out)
}
