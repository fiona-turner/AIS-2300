#### test-make_qemu.R

expect_list <- function(object) {
  expect_vector(object, list())
}

expect_df <- function(object) {
  expect_s3_class(object, "data.frame")
}

#### Use Banana function as simulator

## this copied from ?banana

d <- 8L
temp <- lapply(1:d, function(j) {
  if (j == 3) {
    factor("dog", c("dog", "cat"))
  } else if (j == 6) {
    factor("apple", c("apple", "orange", "pear"))
  } else {
    runif(1L)
  }
})
temp <- as.data.frame(temp, col.names = LETTERS[1L:d])

my_banana <- with(list(temp = temp), function(X) {
  runs <- check_runs(X, template = temp)
  X <- runs$X
  fac <- runs$fac
  for (j in fac) {
    x <- X[[j]]
    lev <- levels(x)
    X[[j]] <- as.numeric(x) / (1 + length(lev))
  }
  apply(as.matrix(X), 1L, banana)
})

## do the runs

n <- 100L

X <- lapply(temp, function(x) {
  if (is.factor(x)) {
    lev <- levels(x)
    x <- lev[sample.int(length(lev), size = n, replace = TRUE)]
    factor(x, lev)
  } else {
    runif(n)
  }
})
X <- as.data.frame(X, col.names = LETTERS[1L:d])

## quick check of check_runs()

test_that("\'check_runs()\' works", {
  expect_list(check_runs(X, template = temp))
})

y <- my_banana(X) # n-vector

#### here we go, first set of tests

test_that("\'make_qemu()\' works", {
  expect_s3_class(emu <- make_qemu(X, y), "qemu")
  expect_s3_class(emu <- make_qemu(X, y, inlogs = TRUE), "qemu")
  expect_s3_class(emu <- make_qemu(X, y, fmla = y ~ . + I(A^2)), "qemu")
})

#### test predictions


