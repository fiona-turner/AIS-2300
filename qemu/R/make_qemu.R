#' Quick Emulator
#'
#' @description
#' Create a *Q*uick *EMU*lator (*qemu*) using a linear model for the regressors and a random forest for the residual.  Can handle factor inputs (also known as 'categorical' or 'discrete' inputs).
#'
#' Setting `inlogs = TRUE` will emulate `log(y + offset)` rather than `y`.  This is internal, the predictions are on the original scale.
#'
#' The `qemu` is built on-top of [stats::lm()], [quantregForest()] from the 'quantregForest' package, and [stats::ecdf()].  'quantregForest' is built on-top of the 'randomForest' package.
#'
#' @details The regressors are specified using the [formula] object exactly as in [stats::lm()].  The default `y ~ .` is a constant, a linear term in each continuous input, and one-hot encoding of each factor.  Add or delete terms as required.  For example, for a squared term in input 'B', do
#' ```
#' emu <- make_qemu(X, y, fmla = y ~ . + I(B^2))
#' ```
#' where the `I()` is required because the caret has a separate meaning (factor crossing).  To remove the linear term in input 'A', do
#'
#' ```
#' emu <- make_qemu(X, y, fmla = y ~ . - A)
#' ```
#' and so on.  See the Examples.
#'
#' @param X Matrix or dataframe of inputs.  Can include factors (for a dataframe).
#' @param y Numeric vector of responses.
#' @param fmla Linear model for the regressors.  See Details and Examples.
#' @param inlogs Logical, should `y` be emulated in logs?
#' @param offset An offset if emulating in logs.
#' @param nthreads Number of cores, not effective on Windows.
#' @param mtry,nodesize Arguments passed through to `randomForest()`, with the same default values.
#' @param ... Other arguments passed through to `randomForest()`, such as `ntree` and `sampsize`.
#' @param object Object of class `qemu`, in `summary()`.
#'
#' @returns
#' * `make_qemu()` returns an object of class `qemu`, a list with components:
#'   * `Runs` : the simulator runs
#'   * `args` : the other function arguments
#'   * `moreargs` : additional arguments passed to `quantregForest()`.
#'   * `lmfit` : the linear model fit for the regressors
#'   * `RFfit` : the random forest fit for the residual
#'
#' @seealso
#'
#' Numerical diagnostics are available from [CRPS_qemu()].  These can be used to tune, say, `mtry` and `nodesize` using a held-out dataset.  See Lin and Jeon (2006) for guidance.  `tune_qemu()` automates the tuning, but it is expensive.
#'
#' Bayesian optimization is available using [bopt_qemu()].
#'
#' @references
#'
#' L. Breiman, 2001, Random Forests, Machine Learning, 45, 5-32.
#'
#' T. Hastie, R. Tibshirani, and J. Friedman, 2009, The Elements of Statistical Learning, Springer, 2nd ed.  Ch15 on Random Forests.
#'
#' Y. Lin and Y. Jeon, 2006, Random Forests and Adaptive Nearest Neighbors, Journal of the American Statistical Association, 101, 578-590.
#'
#' N. Meinhausen, 2006, Quantile Regression Forests, Journal of Machine Learning, 7, 983-999.
#'
#' @examples
#' #### example with the wing_weight() simulator
#'
#' ## ## generate some simulator runs
#'
#' ## simple random design (NOT recommended in practice!)
#'
#' Ranges <- list(
#'   Sw = c(174, 150, 200),
#'   Wfw = c(252, 220, 300),
#'   A = c("nom", "high"),
#'   LamCaps = c(0, -10, 10),
#'   q = c(34, 16, 45),
#'   lam = c("nom", "low", "high"),
#'   tc = c(0.12, 0.08, 0.18),
#'   Nz = c(3.8, 2.5, 6),
#'   Wdg = c(2000, 1700, 2500),
#'   Wp = c(0.05, 0.025, 0.08))
#'
#' ## put the nominal input value at the top
#'
#' n <- 501
#' X <- lapply(Ranges, function(x) {
#'   if (is.character(x)) {
#'     lev <- x
#'     y <- x[sample.int(length(x), size = n - 1, replace = TRUE)]
#'     factor(c(x[1], y), lev)
#'   } else {
#'     y <- runif(n - 1, x[2], x[3])
#'     c(x[1], y)
#'   }
#' }) |> as.data.frame()
#'
#' WW <- wing_weight(X) # list with X (dataframe) and y components
#' y <- WW$y
#'
#' ## ## now process the simulator runs
#'
#' ## MEFF plot directly from the runs
#'
#' meff0 <- MEFF_runs(X, y, runs = WW, main = "Directly from the runs")
#'
#' ## build qemu out of the box
#'
#' emu1 <- make_qemu(X, y)
#' show(summary(emu1)) # just the regression part
#'
#' ## how about emulating in logs
#'   
#' emu2 <- make_qemu(X, y, inlogs = TRUE)
#'
#' ## make a prediction
#'
#' Xnew <- lapply(X, sample, replace = TRUE) |> as.data.frame()
#' Xnew <- head(Xnew, 20)
#' pp1 <- predict(emu1, Xnew)
#' pp1mom <- predict(emu1, Xnew, type = "mom")
#'
#' ## prediction when emulating in logs
#'
#' pp2mom <- predict(emu2, Xnew, type = "mom") # two extra columns
#'
#' ## main effects
#'
#' op <- par(mfrow = c(2, 2))
#'
#' meff1 <- MEFF_qemu(emu1, runs = WW, main = "Original scale", inom = 1)
#' meff2 <- MEFF_qemu(emu2, main = "Emulating in logs", inom = 1)
#'
#' ## LOO diagnostic, uses existing emulator, a bit slow
#'
#' ## 5% of sample, stratify by Nz
#'
#' oo <- order(X$Nz)
#' ho <- oo[seq_along(oo) %% round(1 / 0.05) == 0]
#'
#' par(mfrow = c(2, 2))
#' \dontrun{
#' loo1 <- LOO_qemu(emu1, subset = ho, plotit = FALSE)
#' plot(loo1, along = "Nz", main = "Full, Original scale")
#' loo2 <- LOO_qemu(emu2, subset = ho, along = "Nz",
#'   main = "Full, Emulating in logs")
#' }
#' ## use type = "quick" if you're short of time
#'
#' loo1_q <- LOO_qemu(emu1, subset = ho, type = "quick", along = "Nz",
#'   main = "Quick, Original scale")
#' loo2_q <- LOO_qemu(emu2, subset = ho, type = "quick", along = "Nz",
#'   main = "Quick, Emulating in logs")
#'
#' ## CRPS diagnostic on hold-out -- rebuild emulators
#'
#' emu1_ho <- make_qemu(X[-ho, , drop=FALSE], y[-ho]) # not hold-out
#' crps1_ho <- CRPS_qemu(emu1_ho, X = X[ho, , drop=FALSE], y = y[ho])
#' ylim <- range(c(0, crps1_ho))
#' plot(X$Nz[ho], crps1_ho, pch = 19, ylim = ylim,
#'   main = "CRPS, Original scale")
#'
#' emu2_ho <- make_qemu(X[-ho, , drop=FALSE], y[-ho]) # not hold-out
#' crps2_ho <- CRPS_qemu(emu2_ho, X = X[ho, , drop=FALSE], y = y[ho])
#' plot(X$Nz[ho], crps2_ho, pch = 19, ylim = ylim,
#'   main = "CRPS, Emulating in logs")
#'
#' ## tune the values of mtry and nodesize, a bit slow
#'
#' \dontrun{
#' tune1 <- tune_qemu(emu1) # plots as well
#' emu1_tuned <- tune1$tuned_qemu
#' }
#'
#' ## example of using different regressors
#'
#' fmla <- y ~ . + I(Nz^2) + I(Nz * Sw)
#' emu1_reg <- make_qemu(X, y, fmla = fmla)
#' show(summary(emu1_reg))
#' 
#' @name make_qemu
#' @export
#' @rdname make_qemu

make_qemu <- function(X, y, fmla = y ~ ., 
  inlogs = FALSE, offset = 0,
  nthreads = getOption("mc.cores", 2L),
  mtry = max(1, floor(d / 3)), nodesize = 5, ...) {

  ## sort out inputs and outputs

  Runs <- check_runs(X, y)
  n <- nrow(Runs$X)
  d <- ncol(Runs$X)
  inlogs <- isTRUE(inlogs[1L])
  if (inlogs) {
    offset <- offset[1L]
    y <- log(y + offset)
    if (anyNA(y)) {
      stop("Cannot compute log(y + offset)")
    }
  }
  moreargs <- list(...) # these are going into quantregForest

  ## linear fit

  lmfit <- lm(fmla, data = cbind(X, y = y))
  u <- residuals(lmfit)

  ## RF for residuals

  RFfit <- do.call(quantregForest::quantregForest,
    c(alist(x = X, y = u, nthreads = nthreads, mtry = mtry,
      nodesize = nodesize), moreargs))

  ## package and return

  robj <- list(
    Runs = Runs,
    args = list(
      fmla = fmla,
      inlogs = inlogs,
      offset = offset,
      nthreads = nthreads,
      mtry = mtry,
      nodesize = nodesize),
    moreargs = moreargs,
    lmfit = lmfit,
    RFfit = RFfit)
  class(robj) <- c("qemu", class(robj))
  robj
}

#' @export
#' @rdname make_qemu

summary.qemu <- function(object, ...) {
  summary(object$lmfit)
}

