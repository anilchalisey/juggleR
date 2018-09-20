#' Run command via Bash directly from R
#'
#' Allows users to run Bash commands directly from within R.
#' This will only work in WIndows if WSL has been set up.
#'
#' @param cmd the system command to be invoked, as a character string.
#' @param intern a logical (not NA) which indicates whether to capture the
#'   output of the command as an R character vector.
#' @param wait a logical (not NA) indicating whether the R interpreter
#'   should wait for the command to finish, or run it asynchronously. This
#'   will be ignored (and the interpreter will always wait) if
#'   intern = TRUE.
#'
#' @return
#' If intern = TRUE, a character vector giving the output of the command,
#'   one line per character string. (Output lines of more than 8095 bytes
#'   will be split).  If the command could not be run an R error is generated.
#'   Under the Rgui console intern = TRUE also captures stderr. If command runs
#'   but gives a non-zero exit status this will be reported with a warning and
#'   in the attribute "status" of the result: an attribute "errmsg" may also
#'   be available.
#' If intern = FALSE, the return value is an error code (0 for success), given
#'   the invisible attribute (so needs to be printed explicitly). If the command
#'   could not be run for any reason, the value is 127. Otherwise if wait = TRUE
#'   the value is the exit status returned by the command, and if wait = FALSE it
#'   is 0 (the conventional success value).
#'
#' @export
#'
#' @examples
#' runCmd("ls")
#' runCmd("ls", intern = TRUE)

runCmd <- function(cmd, intern = FALSE, wait = TRUE) {
  if (.Platform$OS.type != "windows") {
    system(command = cmd, intern = intern)
  } else {
    shell(cmd = shQuote(cmd), shell = "bash", intern = intern)
  }
}
