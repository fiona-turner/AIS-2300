#' Bayesian Optimization
#'
#' Use an emulator of a function to score points in the input space according to their expected improvement (bigger is better).
#'
#' @param emu Object of class `qemu`, representing a function to be optimized.
#' @param Z Inputs over which to compute the expected improvement.
#' @param nz If `Z` is missing, generate `nz` points uniformly in the bounding box of the training dataset.
#' @param maximum Logical, maximize?  Default is to _minimize_.
#' @param npts Number of points for the midpoint integration rule.
#'
#' @returns A list with `Z` and `Eimp`, only for those rows where the expected improvement is positive.
#'
#' @details The argument `nz` is provided for 'quick and dirty' optimization, but it will not generate candidate points outside the bounding box of the training dataset -- often this is where the optimum will lie.  It is often better to provide `Z` directly.
#'
#' @references
#'
#' W. I. Notz, 2015, Expected Improvement Designs. Ch19 in A. Dean et al, eds, Handbook of Design and Analysis of Experiments, Chapman & Hall, CRC.
#'
#' @examples
#' ## minimize the Banana function
#'
#' \dontrun{
#' d <- 5
#' n <- 101
#' X <- matrix(runif(n * d), ncol = d)
#' colnames(X) <- LETTERS[1:d]
#' y <- apply(X, 1, banana)
#' emu <- make_qemu(X, y, inlogs = TRUE, nthreads = 2L)
#'
#' bopt <- bopt_qemu(emu)
#' show(bopt) # only those with Eimp > 0
#'
#' ## let's have a look at the predictions
#'
#' pp <- predict(emu, Z = bopt$Z)
#' pp$Eimp <- bopt$Eimp
#' show(pp)
#'
#' ## sequential minimization
#'
#' collapse <- function(x) {
#'   paste(x, collapse = ", ")
#' }
#'
#' nz <- 1E4 # more candidates
#' bopt <- bopt_qemu(emu, nz = nz)
#' niter <- 20
#' for (i in 1:niter) {
#'   x <- unlist(bopt$Z[1, ]) # Z is a dataframe
#'   X <- rbind(X, x)
#'   message("Sim run at ", collapse(signif(x, 3)))
#'   y <- c(y, banana(x))
#'   message("S(x) best so far = ", signif(min(y), 3))
#'   emu <- make_qemu(X, y, inlogs = TRUE)
#'   bopt <- bopt_qemu(emu)
#'   if (length(bopt$Eimp) == 0) {
#'     message("Early exit, no bopt run found")
#'     break
#'   }
#' }
#'
#' }

#' @export

bopt_qemu <- function(emu, Z, nz = 1E3L, maximum = FALSE, npts = 31L) {

  stopifnot(inherits(emu, "qemu"))
  stopifnot(is.logical(maximum), length(maximum) == 1L)

  ## sort out missing Z

  got_Z <- !missing(Z)
  if (!got_Z) {
    nz <- nz[1L]
    stopifnot(nz >= 1, nz == round(nz))
    Z <- lapply(emu$Runs$X, function(x) {
      if (is.factor(x)) {
        lev <- levels(x)
        x <- lev[sample.int(length(lev), size = nz, replace = TRUE)]
        factor(x, lev)
      } else {
        rng <- range(x)
        runif(nz, rng[1L], rng[2L])
      }
    })
  }
  Z <- as.data.frame(Z)

  ## get the ECDF for each row

  pp <- predict(emu, Z = Z, type = "ecdf")

  ## score by expected improvement

  ybsf <- min(emu$Runs$y)
  Eimp <- parallel::mclapply(1L:nz, function(i) {
    ecdf <- pp[[i]]
    rng <- range(knots(ecdf))
    dy <- diff(rng) / npts
    ypts <- seq(from = rng[1L], by = dy, length.out = npts + 1) # breaks
    pp <- diff(ecdf(ypts)) # probabilities
    ypts <- seq(from = rng[1L] + dy / 2, by = dy, length.out = npts) # midpoints
    if (maximum) {
      sum((ypts > ybsf) * (ypts - ybsf) * pp)
    } else {
      sum((ybsf > ypts) * (ybsf - ypts) * pp)
    }
  }, mc.cores = emu$args$nthreads)

  ## and filter to keep those with Eimp > 0

  Eimp <- unlist(Eimp)
  oo <- order(Eimp, decreasing = TRUE)
  oo <- oo[Eimp[oo] > 0]
  list(Z = Z[oo, ], Eimp = Eimp[oo]) # might be empty
}

