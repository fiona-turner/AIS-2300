#' Leave-One-Out (LOO) plot
#'
#' Take a `LOO` object and plot it.  A `LOO` object is a dataframe with `actual`, `predicted`, `lower`, and `upper` components.
#'
#' @param x Object of class `LOO`.
#' @param y Ignored.
#' @param along How to order the x-axis, the name of an input or its index.  Default uses the same ordering as `subset`.
#' @param ylim,log,xlab,ylab,main,cex Usual plotting arguments.
#' @param ... For compatibility with [plot()].
#'
#' @returns Called for its side-effect of producing a Leave One Out (LOO) plot, returns `x` invisibly.
#'
#' @examples
#' ## see ?make_qemu
#'
#' @export

plot.LOO <- function(x, y, along = NULL, ylim = NULL, log = "",
  xlab = NULL, ylab = NULL, main = NULL, cex = 1, ...) {

  LOO <- x
  stopifnot(missing(y))
  class(LOO) <- class(LOO)[-1L] # now a dataframe
  args <- attr(LOO, "args")
  X <- attr(LOO, "X")
  n <- nrow(X)
  d <- ncol(X)

  ## two different styles of x-axis

  if (!is.null(along)) {
    along <- along[1L]
    if (is.character(along)) {
      along <- match(along, names(X))
    }
    stopifnot(length(along) == 1, along %in% 1:d)
    x <- X[, along]
    oo <- order(x)
    x <- x[oo]
    LOO <- LOO[oo, , drop=FALSE]
    if (is.null(xlab)) {
      xlab <- sprintf("Values of %s", names(X)[along])
    }
  } else {
    x <- 1:n
    if (is.null(xlab)) {
      xlab <- ""
    }
  }

  ## here we go

  if (is.null(ylim)) {
    ylim <- my_range(unlist(LOO))
  }
  plot(x, LOO$actual, type = "n", ylim = ylim, log = log, xlab = xlab,
    ylab = ylab, main = main)
  with(LOO, {
    col <- ifelse(lower <= actual & actual <= upper, "blue", "red")
    segments(x, lower, y1 = upper, col = col)
    points(x, predict, pch = 3, col = col)
    points(x, actual, pch = 19, col = "black", cex = cex)
  })

  invisible(x)
}

