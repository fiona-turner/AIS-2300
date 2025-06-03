#' Mallard Dataset
#'
#' @description
#' Computer simulations for two shots on the National Ignition Facility (NIF) at Lawrence Livermore National Laboratory, plus the measurements.
#'
#' @format
#' A list with three components, `200525`, `210210`, and `Meas`.  The first two are simulator runs for each shot, with the format:
#' \describe{
#' \item{times}{Times in ns.}
#' \item{X}{Matrix of input values: one row per run, one column per input.  These are all multipliers on the underlying nominal values, and therefore have nominal value of 1.}
#' \item{F}{Matrix of flux output values: one row per run, one column per time.  The units of flux are ???.}
#' \item{campaign}{Integer showing the batches of the design.}
#' }
#' The final one, `MEAS`, are the measurements from the NIF instrument DANTE2, a dataframe with columns `200525`, and `210210`, and `times`.
#'
#' @source These runs were performed as part of the experiment described in
#'
#' D. Hoarty, J. Morton, J.C. Rougier, M. Rubery, Y.P. Opachich, D. Swatton, S. Richardson, R.F. Heeter, K. McLean, S.J. Rose, T.S. Perry, and B.A. Remington (2023), Radiation burn-through measurements to infer opacity at conditions close to the solar radiative zone-convective zone boundary, _Physics of Plasmas_, 30, 063302. See \url{https://doi.org/10.1063/5.0141850}. 
#'
#' @name Mallard
#' @keywords datasets
NULL

