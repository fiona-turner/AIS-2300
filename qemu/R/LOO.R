#' Leave-One-Out (LOO)
#'
#' LOO analysis for the emulator.  Since random forests are a bit forgetful, a similar result follows from just using the predict method, or `type = "quick"` in the arguments.
#'
#' @param emu Object of class `QEMU`.
#' @param subset The rows to leave out, or `"all"` to do all rows (EXPENSIVE!).
#' @param type Full LOO (EXPENSIVE!) or quick-and-dirty?
#' @param level Coverage of the prediction interval.
#' @param plotit Logical, plot the result?
#' @param ... Passed to [plot.LOO()].
#'
#' @returns A dataframe with `actual`, `predict`, `lower`, and `upper` components, and a `X` attribute with the input values for each row.
#'
#' @examples
#' ## see ?make_qemu
#'
#' @export

LOO_qemu <- function(emu, subset = "all", type = c("full", "quick"),
  level = 0.95, plotit = TRUE, ...) {

  stopifnot(inherits(emu, "qemu"))
  X <- emu$Runs$X
  y <- emu$Runs$y
  n <- nrow(X)
  if (subset[1L] == "all") {
    subset <- 1L:n
  } else {
    stopifnot(subset %in% 1:n, !duplicated(subset))
  }

  args <- emu$args
  args$level <- level
  type <- match.arg(type)

  if (type == "full") {

    ## eval each one in series

    robj <- lapply(subset, function(i) {
      foo <- do.call("make_qemu", c(alist(
        X = X[-i, , drop=FALSE], y = y[-i], fmla = args$fmla,
        inlogs = args$inlogs, offset = args$offset,
        nthreads = args$nthreads, mtry = args$mtry,
        nodesize = args$nodesize), emu$moreargs))
      predict(foo, X[i, , drop=FALSE], type = "interval",
        level = level) # 1-row DF with lower, median, upper
    })
    robj <- do.call("rbind", robj) # one DF

  } else { # type = "quick"

    robj <- predict(emu, X[subset, , drop=FALSE], level = level)

  }

  robj <- data.frame(
    actual = y[subset],
    predict = robj$median,
    lower = robj$lower,
    upper = robj$upper)
  attr(robj, "X") <- X[subset, , drop=FALSE]
  class(robj) <- c("LOO", class(robj))

  if (isTRUE(plotit)) {
    plot(robj, ...)
  }
  robj
}

