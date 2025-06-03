#' Check the Ensemble of Runs
#'
#' Check the `X` and `y` (optional) of an ensemble of runs.
#'
#' @param X Matrix or dataframe of input values.
#' @param y Vector or matrix of output values.
#' @param template Template for checking the inputs.
#'
#' @returns If successful, a list with components `X` (dataframe) and `y` (numeric vector or matrix), and `factors` (character vector of names). Otherwise an error with an informative message.
#'
#' @export
#'

check_runs <- function(X, y = NULL, template = NULL) {

  ## some error handlers

  collapse <- function(x, collapse = ", ") {
    paste(x, collapse = collapse)
  }

  my_stop <- function(...) {
    msg <- c("check_runs()", ...)
    stop(collapse(msg, "\n"), call. = FALSE)
  }

  ## basic checks

  X <- as.data.frame(X)
  n <- nrow(X)
  yok <- is.null(y) || (length(y) == n || (is.matrix(y) && nrow(y) == n))
  if (!yok) {
    my_stop("\'y\' does not match \'X\'")
  }

  ## check for NA

  tmp <- X
  if (!is.null(y)) {
    tmp <- cbind(tmp, y)
  }
  omit <- attr(na.omit(tmp), "na.action")
  if (!is.null(omit)) {
    my_stop(sprintf("NAs detected in rows: %s", collapse(omit)))
  }

  ## poss nothing else to do

  if (is.null(template)) {
    fac <- names(X)[sapply(X, is.factor)]
    return(list(X = X, y = y, factors = fac))
  }

  ## check against the template

  if (!is.data.frame(template)) {
    my_stop("Expecting \'template\' to be a dataframe")
  }

  ## variables must match

  nmx <- names(X)
  nmy <- names(template)
  if (!setequal(nmx, nmy)) {
    notinx <- setdiff(nmy, nmx)
    notiny <- setdiff(nmx, nmy)
    msg <- "Variables don\'t match"
    if (length(notinx) > 0) {
      msg <- c(msg, sprintf("Not in \'X\': %s", collapse(notinx)))
    }
    if (length(notiny) > 0) {
      msg <- c(msg, sprintf("Not in \'template\': %s", collapse(notiny)))
    }
    my_stop(msg)
  }

  ## check factors

  fac <- nmy[sapply(template, is.factor)]
  for (ff in fac) {
    lev <- levels(template[[ff]])
    x <- as.character(X[[ff]])
    x <- factor(x, lev)
    if (anyNA(x)) {
      my_stop(sprintf("Unrecognized level(s) in %s", ff))
    }
    X[[ff]] <- x
  }

  return(list(X = X, y = y, factors = fac))
}

