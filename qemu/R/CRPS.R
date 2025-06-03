#' Continuous Ranked Probability Score
#'
#' Compute the CRPS for a set of simulator runs.
#'
#' @param emu Object of class `qemu`.
#' @param X,y Validation data, inputs and outputs.
#' @param npts Number of points for the midpoint integration rule.
#'
#' @returns A vector of CRPS scores.
#'
#' @examples
#' ## see ?make_qemu

#' @export

CRPS_qemu <- function(emu, X, y, npts = 51) {

  stopifnot(inherits(emu, "qemu"))
  temp <- emu$Runs$X[1L, , drop=FALSE]
  runs <- check_runs(X, y, template = temp)

  ## get the ECDFs

  pp <- predict(emu, Z = runs$X, type = "ecdf")

  ## compute the CRPS using the midpoint rule

  y <- runs$y
  sapply(seq_along(pp), function(i) {
    ecdf <- pp[[i]]
    rng <- range(knots(ecdf))
    dy <- diff(rng) / npts
    ypts <- seq(from = rng[1L] + dy / 2, by = dy, length.out = npts)
    sum(dy * ((ypts >= y[i]) - pp[[i]](ypts))^2)
  })
}
