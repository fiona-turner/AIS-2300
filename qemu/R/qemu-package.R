#' qemu package
#'
#' @description
#' Train an emulator on a set of computer simulator runs, in order to predict output values at new input values.  Uses linear regressors, and a random forest for the residual process.  Can handle factor inputs.
#'
#' * `make_qemu()` makes an emulator from a training dataset.
#' * `predict()` predicts the simulator at new input values.
#' * `CRPS_qemu()` scores the simulator on a validation dataset.
#' * `tune_qemu()` tunes the two parameters `mtry` and `nodesize`.
#' * `LOO_qemu()` does a leave-one-out validation analysis.
#' * `MEFF_qemu()` computes the main effects.
#' * `bopt_qemu()` is for Bayesian minimization.
#'
#' A 'qemu' is a cross between a kiwi and an emu.
#'
#' @author Jonathan Rougier
#'
#' @name qemu
#' @docType package
#'
#' @importFrom stats dist residuals ecdf knots runif lm median predict sd loess na.omit qnorm
#' @importFrom methods show
#' @importFrom utils head tail packageVersion
#' @importFrom graphics image par rect text axis grid lines polygon title abline plot.new plot.window points segments
#' @importFrom grDevices terrain.colors col2rgb hcl.colors rgb

NULL

