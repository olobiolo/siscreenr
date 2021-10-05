#' rerun a function call in case of error
#'
#' If a call returns an error, run it again.
#'
#' Iterative calling of a function that can return a random error
#' can crash a process or at the very least result in incomplete data.
#' To alleviate the impact of random errors the failed call will be rerun
#' until it succeeds or until a limit on the number of attempts is reached.
#' If all attempts fail, a backup value will be returned.
#'
#' @param call call to retry
#' @param max.iter maximum number of iterations after which failure is declared
#' @param fail object to return in case of failure
#' @param verbose logical flag whether to print iterations as messages,
#' @param ... ellipsis is to facilitate using \code{retry} within functions
#'            and retain control over verbosity
#'
#' @return \code{retry} returns the same as the function in \code{call}
#' or as defined by \code{fail}.
#'
#' @section Source:
#' This function is lifted from package
#' \href{http://github.com/olobiolo/acutils}{acutils}.
#'
#' @export
#'
#' @examples
#' f <- function() {
#'   x <- sample(1:2, size = 1)
#'   if (x == 1) stop("it's a 1", call. = TRUE)
#'   return(x)
#' }
#' \dontrun{
#' f()
#' replicate(10, f())
#' }
#'
#' retry(f(), 2, "failed", TRUE)
#' table(replicate(100, retry(f(), 2, "failed", FALSE)))
#'
#' @seealso \code{\link[base]{dots}}

retry <- function(call, max.iter, fail = NA, verbose = FALSE, ...) {
  C <- substitute(call)
  if (!is.call(C)) stop('"call" must be a function call')
  if (!is.numeric(max.iter)) stop('"max.iter" must be numeric')
  if (max.iter > 1000) warning('a large number of attempts has been requested')

  # define constructor for counting runner
  G <- function() {
    iter <- 0
    g <- function() {
      iter <<- iter +1
      if (verbose & iter > 1) message('retry at ', deparse(C), ';\t attempt ', iter)
      tryCatch(eval(C, parent.frame(n = 2)),
               error = function(e) {
                 if (iter < max.iter) g() else {
                   if (verbose) message('all attempts failed')
                   return(fail)}})
    }
    return(g)
  }
  # run constructor to create counting runner
  h <- G()
  # run counting runner
  h()
}
