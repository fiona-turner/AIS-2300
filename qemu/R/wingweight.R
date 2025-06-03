#' Wing Weight Function
#'
#' The _wing weight_ function using the implementation from <https://www.sfu.ca/~ssurjano/Code/wingweightr.html>, adapted to change two of the inputs into factors (also know as 'categorical' or 'discrete' inputs).
#'
#' @param ... Input values, see Inputs and Examples.
#' @param TYPE Type of object for the inputs in the function output.
#' @param ALL Logical, return all the inputs or just the ones in `...`.
#'
#' @section Inputs:
#' The named arguments in `...` are combined into a dataframe, and then the columns in that dataframe are used to overwrite the nominal values for each input.  The inputs and their default values and ranges are:
#'
#' \tabular{lll}{
#' Name \tab Description \tab Values (nominal, range) \cr
#' Sw \tab Wing area (ft^2) \tab 174, 150 to 200 \cr
#' Wfw \tab Weight of fuel in wing (lb) \tab 252, 220 to 300 \cr
#' A \tab Aspect ratio \tab Factor, 'nom' and 'high' \cr
#' LamCaps \tab Quarter-chord sweep (deg) \tab 0, -10 to 10 \cr
#' q \tab Dynamic pressure at cruise (lb/ft^2) \tab 34, 16 to 45 \cr
#' lam \tab Taper ratio \tab Factor, 'nom', 'low', 'high' \cr
#' tc \tab Aerofoil thickness to chord ratio \tab 0.12, 0.08 to 0.18 \cr
#' Nz \tab Ultimate load factor \tab 3.8, 2.5 to 6 \cr
#' Wdg \tab Final design gross weight (lb) \tab 2000, 1700 to 2500 \cr
#' Wp \tab Paint weight (lb/ft^2) \tab 0.05, 0.025 to 0.08 \cr
#' }
#'
#'
#' @returns A list with `X` and `y` components.
#'
#' @examples
#' ## nominal values
#'
#' ww <- wing_weight() # ALL = TRUE in this case
#' ww <- wing_weight(TYPE = "matrix") # factors become numeric
#' 
#' ## can be called with a subset of inputs
#' 
#' Sw <- seq(from = 150, to = 200, length.out = 11)
#' ww <- wing_weight(Sw = Sw)
#' 
#' ## or a dataframe
#'
#' X <- expand.grid(Sw = Sw, A = c("nom", "high"))
#' ww <- wing_weight(X)
#'
#' ## some other combo
#'
#' ww <- wing_weight(X, LamCaps = 5)
#'
#' ## get back all the input values
#'
#' ww <- wing_weight(X, LamCaps = 5, ALL = TRUE)
#'
#' @export

wing_weight <- function(..., TYPE = c("dataframe", "matrix"), ALL = FALSE) {

  ## check inputs

  if (...length() > 0) {
    df <- data.frame(...)
    if (!is.null(df$A)) {
      df$A <- factor(df$A, c("nom", "high"))
      if (anyNA(df$A)) {
        stop("Unrecognized level(s) in \'A\'")
      }
    }
    if (!is.null(df$lam)) {
      df$lam <- factor(df$lam, c("nom", "low", "high"))
      if (anyNA(df$lam)) {
        stop("Unrecognized level(s) in \'lam\'")
      }
    }
  } else {
    df <- NULL
    ALL <- TRUE # always return something in X
  }

  TYPE <- match.arg(TYPE)
  nms <- names(df)

  ## default values

  df0 <- data.frame(
    Sw = 174,
    Wfw = 252,
    A = factor("nom", c("nom", "high")),
    LamCaps = 0,
    q = 34,
    lam = factor("nom", c("nom", "low", "high")),
    tc = 0.12,
    Nz = 3.8,
    Wdg = 2000,
    Wp = 0.05
  )

  if (is.null(df)) {
    df <- df0
  } else {
    df0 <- df0[rep(1L, nrow(df)), ]
    for (nn in names(df0)) {
      if (nn %in% names(df)) {
        df0[[nn]] <- df[[nn]]
      }
    }
    df <- df0
  }
  row.names(df) <- NULL
  row.names(df0) <- NULL

  ## overwrite the factors and call the simulator

  df$A <- c(7.52, 10)[as.numeric(df$A)]
  df$lam <- c(0.672, 0.5, 1)[as.numeric(df$lam)]
  X <- as.matrix(df)
  y <- apply(X, 1L, wingweight)

  ## package and return

  robj <- list(X = X, y = y)
  if (isFALSE(ALL)) {
    robj$X <- robj$X[, nms, drop=FALSE]
  }
  if (TYPE == "dataframe") {
    robj$X <- df0
    if (isFALSE(ALL)) {
      robj$X <- robj$X[, nms]
    }
  }
  robj
}

## original function

wingweight <- function(xx)
{
  ##########################################################################
  #
  # WING WEIGHT FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # y  = wing weight
  # xx = c(Sw, Wfw, A, LamCaps, q, lam, tc, Nz, Wdg, Wp)
  #
  ##########################################################################
  
  Sw      <- xx[1]
  Wfw     <- xx[2]
  A       <- xx[3]
  LamCaps <- xx[4] * (pi/180)
  q       <- xx[5]
  lam     <- xx[6]
  tc      <- xx[7]
  Nz      <- xx[8]
  Wdg     <- xx[9]
  Wp      <- xx[10]
  
  fact1 <- 0.036 * Sw^0.758 * Wfw^0.0035
  fact2 <- (A / ((cos(LamCaps))^2))^0.6
  fact3 <- q^0.006 * lam^0.04
  fact4 <- (100*tc / cos(LamCaps))^(-0.3)
  fact5 <- (Nz*Wdg)^0.49
  
  term1 <- Sw * Wp
  
  y <- fact1*fact2*fact3*fact4*fact5 + term1
  return(y)
}
