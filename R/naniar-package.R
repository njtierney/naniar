#' naniar
#'
#' naniar is a package to make it easier to summarise and handle missing values
#' in R. It strives to do this in a way that is as consistent with tidyverse
#' principles as possible.
#'
#' @name naniar
#' @docType package
#' @seealso [add_any_miss]() [add_label_missings]() [add_label_shadow]() [add_miss_cluster]() [add_n_miss]() [add_prop_miss]() [add_shadow]() [add_shadow_shift]() [as_shadow]() [bind_shadow]() [cast_shadow]() [cast_shadow_shift]() [cast_shadow_shift_label]() [draw_key_missing_point]() [gather_shadow]() [geom_miss_point]() [gg_miss_case]() [gg_miss_case_cumsum]() [gg_miss_fct]() [gg_miss_span]() [gg_miss_var]() [gg_miss_var_cumsum]() [gg_miss_which]() [label_miss_1d]() [label_miss_2d]() [label_missings]() [miss_case_pct]() [miss_case_prop]() [miss_case_summary]() [miss_case_table]() [miss_prop_summary]() [miss_scan_count]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]() [prop_miss_row]() [replace_to_na]() [replace_with_na]() [replace_with_na_all]() [replace_with_na_at]() [replace_with_na_if]() [shadow_shift]() [stat_miss_point]() [vis_miss]() [where_na]()

#' @import ggplot2
#' @import rlang
NULL

#' @importFrom visdat vis_miss
#' @export
visdat::vis_miss


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
