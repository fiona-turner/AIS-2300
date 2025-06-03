#' Tune 'mtry' and 'nodesize'
#'
#' Compute CRPS on a hold-out sample for a range of values for `mtry` and `nodesize`, and optionally plot these in a grid.
#'
#' @param emu Object of class `qemu`.
#' @param prop_train Proportion of the runs used for training.
#' @param nrep Number of replications for each combination of `mtry` and `nodesize`.
#' @param mtry_vals Values for `mtry`.
#' @param nodesize_vals Values for `nodesize`.
#' @param plotit Logical, plot the result?
#' @param x Object of class `tune_qemu` for plot method.
#' @param y,... For compatiility with [plot()].
#' @param cex Expansion factor for text values in the plot.
#'
#' @returns A list with components:
#' * `itrain` : indices of the training dataset.
#' * `grid` : grid of values for `mtry` and `nodesize`.
#' * `crps` : mean CRPS score over the hold-out sample.
#' * `best` : best values of `mtry` and `nodesize`, as a list.
#' * `tuned_qemu` : qemu with best parameters, refitted for all the runs.
#'
#' @seealso [CRPS_qemu()] for calculating the CRPS.
#'
#' @examples
#' ## see ?make_qemu

#' @name tune_qemu
#' @export
#' @rdname tune_qemu

tune_qemu <- function(emu, prop_train = 0.9, nrep = 10L,
  mtry_vals = 1L:min(d, 8), nodesize_vals = 1L:8, 
  plotit = TRUE, ...) {

  stopifnot(inherits(emu, "qemu"))
  X <- emu$Runs$X
  n <- nrow(X)
  d <- ncol(X)
  y <- emu$Runs$y
  args <- emu$args

  ## randomly split the dataset

  ii <- sort(sample.int(n, size = ceiling(prop_train * n)))
  Xtrain <- X[ii, , drop=FALSE]
  ytrain <- y[ii]
  Xvalid <- X[-ii, , drop=FALSE]
  yvalid <- y[-ii]

  ## all of the possibilities

  grid <- expand.grid(mtry = mtry_vals, nodesize = nodesize_vals)
  G <- as.matrix(grid)
  crps <- apply(G, 1L, function(g) {
    foo <- rep(0, length(yvalid))
    for (i in 1L:nrep) {
      emu <- do.call("make_qemu", c(alist(
        X = Xtrain, y = ytrain, fmla = args$fmla,
        inlogs = args$inlogs, offset = args$offset,
        nthreads = args$nthreads, mtry = g["mtry"],
        nodesize = g["nodesize"]), emu$moreargs))
      foo[] <- foo + CRPS_qemu(emu, X = Xvalid, y = yvalid)
    }
    foo / nrep
  })
  mean_crps <- colMeans(crps)
  best <- which.min(mean_crps)
  best <- as.list(G[best, ])

  robj <- list(itrain = ii, grid = grid, crps = mean_crps, 
    best = best,
    tuned_qemu = make_qemu(X = X, y = y, mtry = best$mtry,
      nodesize = best$nodesize, ...))
  class(robj) <- c("tuned_qemu", class(robj))

  ## plot if required

  if (isTRUE(plotit[1L])) {
    plot(robj)
  }
  robj
}

#' @export
#' @rdname tune_qemu

plot.tuned_qemu <- function(x, y, cex = 0.9, ...) {

    stopifnot(missing(y))
    grid <- x$grid
    mtry_vals <- unique(grid$mtry)
    nodesize_vals <- unique(grid$nodesize)
    z <- matrix(x$crps, ncol = length(nodesize_vals))
    prop_train <- x$prop_train
    best <- which.min(as.vector(z))

    breaks <- pretty(z, 12, eps = 1)
    col <- terrain.colors(length(breaks) - 1L)
    image(mtry_vals, nodesize_vals, z, breaks = breaks, col = col,
      xlab = "Values for \'mtry\'", ylab = "Values for \'nodesize\'",
      main = sprintf("Mean CRPS on %.0f%% hold-out", 100 * (1 - prop_train)),
      axes = FALSE)
    axis(1, mtry_vals, lwd = 0, lwd.ticks = 1)
    axis(2, nodesize_vals, las = 1, lwd = 0, lwd.ticks = 1)
    labels <- formatC(z, format = "g", digits = 3)
    text(grid$mtry, grid$nodesize, labels, cex = cex * par("cex.axis"))
    ij <- arrayInd(best, dim(z))
    tmp <- c(mtry_vals[ij[1L]], nodesize_vals[ij[2L]])
    rect(tmp[1L] - 0.5, tmp[2L] - 0.5, tmp[1L] + 0.5, tmp[2L] + 0.5)
    invisible(x)
}

