#' naniar
#'
#' naniar is a package to make it easier to summarise and handle missing values
#' in R. It strives to do this in a way that is as consistent with tidyverse
#' principles as possible.
#'
#' @name naniar
#' @docType package
#' @import ggplot2
#' @import rlang
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
globalVariables(
  c(
    ".temp",
    ".temp_label",
    "rows",
    "..missing..",
    "n_miss",
    "case",
    "variable",
    "value",
    "span_counter",
    "n",
    "n_miss",
    "nheight",
    "pct_miss",
    "n_miss_in_case",
    "values",
    "n_miss_in_case",
    "n_miss_in_var",
    "n_vars",
    "span_every",
    "n_miss_cumsum",
    "as.formula"
    )
  )
