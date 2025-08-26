# R/utils.R

# null-coalesce
`%||%` <- function(x, y) if (is.null(x)) y else x

# drop NULL list elements (handy for paws configs)
compact <- function(x) x[!vapply(x, is.null, logical(1))]
