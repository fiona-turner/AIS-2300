#' Add a 'Sleeve' to a Plot
#'
#' A 'sleeve' is a polygon showing, for example, pointwise uncertainty.  It is used in [plot.MEFF()].
#'
#' @param x Abscissa values.
#' @param lower,upper pointwise lower and upper values corresponding to `x`.
#' @param col colour of the sleeve.
#' @param alpha opacity of the sleeve.
#'
#' @returns Called for its side-effect of adding a sleeve to a plot.
#'
#' @seealso [scales::alpha()] for handling opacity.
#'
#' @examples
#' ## see ?make_qemu, used in plot.MEFF()
#'
#' @export

sleeve <- function(x, lower, upper, col = "black", alpha = 0.3) {
  stopifnot(diff(x) >= 0)
  col <- scales::alpha(col[1L], alpha[1L])
  polygon(c(x, rev(x)), c(lower, rev(upper)), col = col, border = NA)
  invisible(NULL)
}

