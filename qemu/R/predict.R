#' Predictions for the 'qemu' Emulator
#'
#' Predict the simulator output at any input values.  Three types of prediction are available:
#' * `type = "interval"` : a dataframe with `lower`, `median`, and `upper` columns.
#' * `type = "moments"` : a dataframe with `mean` and `sd` columns, plus additional columns `meanlog` and `sdlog` if `object` is made using `inlogs = TRUE`.
#' * `type = "ecdf"` : a list of empirical distribution functions.
#'
#' @param object Object of class `qemu`.
#' @param Z New inputs to predict, must have the same column names and columns types as `X`.
#' @param type Type of prediction, see above.
#' @param level Coverage of the prediction interval when `type = "interval"`.
#' @param ... For compatibilty with [predict()].
#'
#' @returns A dataframe or a list, see above.
#'
#' @seealso [CRPS_qemu()] which uses `type = "ecdf"` to compute diagnostic information using the Continuous Ranked Probability Score.
#'
#' @examples
#' ## see ?make_qemu
#'
#' @export

predict.qemu <- function(object, Z, type = c("interval", "moments", "ecdf"),
  level = 0.95, ...) {

  type <- match.arg(type)
  inlogs <- object$args$inlogs
  offset <- object$args$offset
  factors <- object$Runs$factors

  ## sort out Z

  temp <- object$Runs$X[1L, , drop=FALSE]
  Z <- check_runs(Z, template = temp)$X

  ## interval prediction

  if (type == "interval") {

    stopifnot(0 < level, level < 1)
    alpha <- 1 - level
    what <- c(alpha / 2, 0.5, 1 - alpha / 2)
    u <- predict(object$RFfit, Z, what = what)
    colnames(u) <- c("lower", "median", "upper")
    robj <- u + unname(predict(object$lmfit, newdata = Z)) 
    if (inlogs) {
        robj[] <- exp(robj) - offset
    }
    return(as.data.frame(robj)) # a dataframe
  }

  ## moment prediction

  if (type == "moments") {

    what <- function(x) {c(mean(x), sd(x))}
    u <- predict(object$RFfit, Z, what = what) # matrix with two columns
    robj <- data.frame(
      mean = u[, 1L] + unname(predict(object$lmfit, newdata = Z)),
      sd = u[, 2L])
    if (inlogs) {
      robj$meanlog <- robj$mean
      robj$sdlog <- robj$sd
      robj$varlog <- robj$sdlog^2
      robj$mean <- with(robj, exp(meanlog + varlog / 2))
      robj$cv <- with(robj, sqrt(exp(varlog) - 1))
      robj$sd <- with(robj, cv * mean)
      robj$varlog <- NULL
      robj$cv <- NULL
      robj$mean <- robj$mean - offset
    }
    return(robj) # a dataframe
  }

  ## ECDF prediction

  if (type == "ecdf") {

    u <- predict(object$RFfit, Z, what = ecdf)
    reg <- unname(predict(object$lmfit, newdata = Z))
    return(parallel::mclapply(seq_along(u), function(i) {
      y <- knots(u[[i]]) + reg[i]
      if (inlogs) {
        y[] <- exp(y) - offset
      }
      ecdf(y)
    }, mc.cores = object$args$nthreads)) # a list
  }

  stop("Never get here!")
}

