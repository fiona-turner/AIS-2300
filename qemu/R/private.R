#### private.R : helper functions that are not exported

is.scalar <- function(x) {
  length(x) == 1 && is.numeric(x) && !is.na(x)
}

is.pos.int <- function(x) {
  is.scalar(x) && x == round(x) && x >= 1
}

my_range <- function(x, eps = 1, ...) {
  zapsmall(range(pretty(x, eps = eps, ...)))
}

## for squashing a vector of names

collapse <- function(x, sep = ", ") {
  paste(x, collapse = sep)
}

## list of indices with same values

my_duplicated <- function(x) {
  dup <- duplicated(x)
  if (!any(dup)) {
    return(list())
  }
  robj <- lapply(which(dup), function(i) {
    which(x==x[i])
  })
  hash <- sapply(robj, paste, collapse = ":")
  robj[!duplicated(hash)]
}


