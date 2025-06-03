#' Compute Main Effects
#'
#' @description  The main effect of the jth input is the simulator output when all but the jth input are at their nominal values, and the jth input is varied across its range, taken to be from the lowest to highest values in the training dataset.
#'
#' * `MEFF_qemu()` : compute the MEFFs from a `qemu`.
#' * `MEFF_runs()` : compute the MEFFS directly from the runs.
#'
#' The first is likely to be clearer.  `MEFF_runs()` just does marginal fit using [stats::loess()] with its default arguments.
#'
#' @param object Object of class `QEMU`.
#' @param inom Row-number of the nominal value in the training ensemble.
#' @param level Coverage of the pointwise prediction interval.
#' @param npts Number of points in each main effect.
#' @param plotit Logical, produce a Main Effects plot?
#' @param ... arguments passed to [plot.MEFF()], used if `plotit = TRUE`.
#' @param X,y Runs for `MEFF_runs()`.
#'
#' @returns `MEFF_runs()` and `MEFF_qemu()` create an object of class `MEFF`, mainly for its side-effect of producing a Main Effects plot via the plot method.  See [plot.MEFF()].
#'
#' @examples
#' ## See ?make_qemu
#'
#' @name meffs
#' @export
#' @rdname meffs

MEFF_qemu <- function(object, inom = NA, level = 0.95, npts = 51,
  plotit = TRUE, ...) {

  stopifnot(inherits(object, "qemu"))
  X <- object$Runs$X

  ## stump of nominal values

  inom <- inom[1L]
  if (!is.na(inom)) {
    stopifnot(inom %in% 1:nrow(X))
    stump <- X[inom, , drop=FALSE]
    ynom <- object$Runs$y[inom]
  } else {
    stump <- lapply(X, function(x) {
      if (is.factor(x)) {
        levels(x)[1L]
      } else {
        median(x)
      }
    }) |> as.data.frame()
    ynom <- predict(object, Z = stump)$median
  }
  nominal <- list(x = stump, y = ynom)

  ## run along the outputs

  robj <- lapply(seq_along(X), function(j) {
    x <- X[[j]]
    if (is.factor(x)) {
      z <- factor(levels(x), levels(x))
    } else {
      rng <- range(x)
      z <- seq(from = rng[1L], to = rng[2L], length.out = npts)
    }
    Z <- stump[rep(1L, length(z)), , drop=FALSE]
    Z[[j]] <- z
    pp <- predict(object, Z = Z, type = "interval", level = level)
    data.frame(x = z, predict = pp$median, lower = pp$lower, upper = pp$upper)
  })
  names(robj) <- names(X)
  attr(robj, "nominal") <- nominal
  class(robj) <- c("MEFF", class(robj))

  if (isTRUE(plotit)) {
    plot(robj, ...)
  }
  robj
}

#' @export
#' @rdname meffs

MEFF_runs <- function(X, y, level = 0.95, npts = 51, plotit = TRUE, ...) {

  Runs <- check_runs(X, y)
  X <- Runs$X
  y <- Runs$y

  ## run along the outputs

  robj <- lapply(seq_along(X), function(j) {
    x <- X[[j]]
    if (is.factor(x)) {
      z <- factor(levels(x), levels(x))
      FUN <- function(x) {
        n <- length(x)
        c(predict = mean(x), sd = sd(x) / sqrt(n))
      }
      pp <- tapply(y, x, FUN = FUN)
      pp <- do.call("rbind", pp)
      pp <- cbind(data.frame(x = z), as.data.frame(pp))
    } else {
      lo <- loess(y ~ x, data = data.frame(x = x, y = Runs$y))
      rng <- range(x)
      z <- seq(from = rng[1L], to = rng[2L], length.out = npts)
      pp <- predict(lo, newdata = data.frame(x = z), se = TRUE)
      pp <- data.frame(x = z, predict = pp$fit, sd = pp$se)
    }
    tail <- (1 - level) / 2
    qq <- qnorm(c(tail, 1 - tail))
    pp$lower <- with(pp, predict + qq[1L] * sd)
    pp$upper <- with(pp, predict + qq[2L] * sd)
    pp
  })
  names(robj) <- names(X)
  class(robj) <- c("MEFF", class(robj))

  if (isTRUE(plotit)) {
    plot(robj, ...)
  }
  robj
}
