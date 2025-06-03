#' Banana Simulator
#'
#' @description A general purpose scalar simulator based on extending Rosenbrock's banana function.  The two variants are "A" and "B", see Kok and Sandrock (2009).  "A" has a single stationary point at its global minimum of x = 1.  "B" has many stationary points, especially as the number of inputs becomes larger than about ten.  I have set the default variant to "B" as it is more interesting.
#'
#' The banana function evaluates on the input space R^d, but in practice it suffices to consider \[0, 1\]^d.
#'
#' See Examples for how the banana function can be turned into a toy simulator with both continuous and factor inputs.
#'
#' @param x Vector of input values, length d.
#' @param variant Type of multivariate extension.
#'
#' @returns A scalar value, non-negative and almost certainly positive.
#'
#' @references
#'
#' S. Kok and C. Sandrock, 2009, Locating and Characterizing the Stationary Points of the Extended Rosenbrock Function, _Evolutionary Computing_, 17(3), 437-453.
#'
#' @examples
#' #### Banana simulator with mixed continuous and discrete inputs
#'
#' ## set up a template for the inputs
#'
#' d <- 8L
#' temp <- lapply(1:d, function(j) {
#'   if (j == 3) {
#'     factor("dog", c("dog", "cat"))
#'   } else if (j == 6) {
#'     factor("apple", c("apple", "orange", "pear"))
#'   } else {
#'     runif(1L)
#'   }
#' })
#' temp <- as.data.frame(temp, col.names = LETTERS[1L:d])
#'
#' ## create the Banana simulator from this template
#'
#' my_banana <- with(list(temp = temp), function(X) {
#'   runs <- check_runs(X, template = temp)
#'   X <- runs$X
#'   fac <- runs$fac
#'   for (j in fac) {
#'     x <- X[[j]]
#'     lev <- levels(x)
#'    X[[j]] <- as.numeric(x) / (1 + length(lev))
#'   }
#'   apply(as.matrix(X), 1L, banana)
#' })
#'
#' ## function to generate a random design matrix
#'
#' rbana <- with(list(temp = temp), function(n) {
#'   X <- lapply(temp, function(x) {
#'     if (is.factor(x)) {
#'       lev <- levels(x)
#'       x <- lev[sample.int(length(lev), size = n, replace = TRUE)]
#'       factor(x, lev)
#'     } else {
#'       runif(n)
#'     }
#'   })
#' as.data.frame(X, col.names = LETTERS[1L:d])
#'})
#'
#' ## example
#'
#' X <- rbana(21)
#' y <- my_banana(X)
#' runs <- check_runs(X, y, temp) # all OK
#'
#' @export

banana <- function(x, variant = c("B", "A")) {

  if (!is.numeric(x) || anyNA(x)) {
    stop("odd \'x\': ", paste(x, collapse = ", "))
  }
  variant <- match.arg(variant)

  ## pad to at least 2 inputs, and even if variant A

  d <- length(x)
  if (d == 1 || ((variant == "A") && (d %% 2 == 1))) {
    x <- c(x, 1)
    d[] <- d + 1L
  }

  ## original 2D banana function

  R <- function(x, y) {
    100 * (x^2 - y)^2 + (x - 1)^2
  }

  if (variant == "A") {

    B <- function(x) {
      sum(sapply(seq.int(d / 2), function(i) {
        R(x[2*i - 1], x[2*i])
      }))
    }

  } else { ## variant B

    B <- function(x) {
      ii <- seq.int(d - 1L)
      sum(100 * (x[ii]^2 - x[ii+1L])^2 + (x[ii] - 1)^2)
    }
  }

  B(x)
}
