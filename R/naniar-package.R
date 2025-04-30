#' @description
#' naniar is a package to make it easier to summarise and handle missing values
#' in R. It strives to do this in a way that is as consistent with tidyverse
#' principles as possible.  The work is fully discussed at Tierney & Cook (2023)
#' <doi:10.18637/jss.v105.i07>.
#'
#' @name naniar
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @import ggplot2
#' @importFrom stats median
#' @import rlang
## usethis namespace: end
NULL


if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
globalVariables(
  c(
    "median",
    "variable_NA",
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
    "n_in_span",
    "nheight",
    "pct_miss",
    "n_miss_in_case",
    "values",
    "n_miss_in_case",
    "n_miss_in_var",
    "n_vars",
    "span_every",
    "n_miss_cumsum",
    "as.formula",
    "complete.cases"
  )
)
