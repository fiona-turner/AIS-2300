#' Main Effects Plot
#'
#' @description
#' Take a `MEFF` object and plot it.  A `MEFF` object is a named list of dataframes, one for each input.  Each dataframe has components `x`, `predict`, `lower`, `upper`.
#'
#' To tweak the plot, adjust the arguments in [par()].
#'
#' @param x An object of class `MEFF`.
#' @param y Ignored.
#' @param runs Optional list with `X` and `y` components.  Underplots the main effects with the runs.  Add `pch`, `cex`, and `col` components to `runs` to overwrite default values.
#' @param select Which main effects to plot.
#' @param ylim,log,main,cex.main Usual plot arguments.
#' @param nc How many columns to use in the layout.
#' @param col Colours for each component in `select`.
#' @param alpha 'Fade' for the colour of the uncertainty sleeve, use `alpha = 0` to suppress the sleeve.
#' @param ... For compatiblity with [plot()].
#'
#' @returns Called for its side-effect of producing a Main Effects plot, returns `x` invisibly.
#'
#' @seealso [MEFF_runs()] and [MEFF_qemu()] create `MEFF` objects.
#'
#' @examples
#' ## See ?make_qemu
#'
#' @export

plot.MEFF <- function(x, y, runs, select = 1L:d, ylim = NULL, log = "",
  main = NULL, cex.main = 1.2, nc = NA, col = NA, alpha = 0.3, ...) {

  meff <- x
  nominal <- attr(x, "nominal")
  stopifnot(missing(y))
  d <- length(meff)
  if (is.character(select)) {
    select <- match(select, names(meff))
  }
  stopifnot(select %in% 1:d, !duplicated(select))
  ds <- length(select)
  if (is.na(nc)) {
    nc <- ceiling(sqrt(ds))
  }
  nr <- ceiling(ds / nc)
  if (!is.null(main)) {
    oma <- c(0, 0, 2, 0)
  } else {
    oma <- c(0, 0, 0, 0)
  }
  op <- par(mfrow = c(nr, nc), oma = oma, cex.main = cex.main)
  on.exit(par(op))

  ## sort out runs

  got_runs <- !missing(runs)
  if (got_runs) {
    temp <- lapply(meff, function(df) {
      df$x[1L]
    }) |> as.data.frame()
    check_runs(runs$X, runs$y, template = temp)
    runs_pch <- ifelse(is.null(runs$pch), 19, runs$pch)
    runs_cex <- ifelse(is.null(runs$cex), 1, runs$cex)
    runs_col <- ifelse(is.null(runs$col), scales::alpha("black", 0.3),
      runs$col)
  }

  ## common y-scale

  if (is.null(ylim)) {
    ylim <- lapply(meff, function(x) {
      c(x$lower, x$upper)
    })
    ylim <- unlist(ylim)
    if (got_runs) {
      ylim <- c(ylim, runs$y)
    }
    ylim <- my_range(ylim)
  } else {
    stopifnot(length(ylim) == 2, diff(ylim) > 0)
  }

  ## run along meff

  nms <- names(meff)
  if (is.na(col)) {
    col <- hcl.colors(d, palette = "Dark 3")
  } else {
    col <- rep(col, length.out = d)
  }
  for (j in select) {
    df <- meff[[j]]
    plot.new()
    if (is.factor(df$x)) {
      lev <- as.character(df$x)
      df$x <- as.numeric(df$x)
      plot.window(xlim = range(df$x) + c(-0.25, 0.25), ylim = ylim,
        log = log)
      axis(1, df$x, lev, lwd = 0, lwd.ticks = 1)
      axis(2)
      grid(nx = 0, ny = NULL)
      if (got_runs) {
        points(as.numeric(runs$X[[j]]), runs$y, pch = runs_pch,
          cex = runs_cex, col = runs_col)
      }
      abline(h = nominal$y, lty = "dashed") # ignored if no nominal
      segments(df$x, df$lower, y1 = df$upper, col = col[j], lwd = 3)
      points(df$x, df$predict, pch = 3, col = col[j], lwd = 3)
    } else {
      plot.window(xlim = my_range(df$x), ylim = ylim, log = log)
      axis(1)
      axis(2)
      grid()
      if (got_runs) {
        points(runs$X[[j]], runs$y, pch = runs_pch, cex = runs_cex,
          col = runs_col)
      }
      abline(h = nominal$y, lty = "dashed")      # ignored if 
      abline(v = nominal$x[[j]], lty = "dashed") # no nominal
      if (alpha > 0) {
        sleeve(df$x, df$lower, df$upper, col = col[j], alpha = alpha)
      }
      lines(df$x, df$predict, col = col[j], lwd = 2)
    }
    title(main = nms[j], cex.main = 1)
  }
  if (!is.null(main)) {
    title(main = main, outer = TRUE)
  }

  invisible(x)
}

