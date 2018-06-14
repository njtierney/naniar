#' Test if input is or are shadow variables
#'
#' Creating shadow variables can be done with `bind_shadow`. To see if there
#'   are any shadow variables, `is_shadow` will do this.
#'
#' @param x a vector or data.frame
#'
#' @return logical vector of length 1
#'
#' @examples
#'
#' aq_sh <- as_shadow(airquality)
#' aq_bind <- bind_shadow(airquality)
#'
#' is_shadow(aq_sh)
#' are_shadow(aq_sh)

#' is_shadow(aq_bind)
#' are_shadow(aq_bind)
#'
#' @export
#' @name is_shadow

is_shadow <- function(x){
  inherits(x, "shadow")
}


#' @export
#' @rdname is_shadow
are_shadow <- function(x){
  purrr::map(x, class) %>%
    tibble::as_tibble() %>%
    purrr::map_lgl(~any(grepl("shadow",.)))
}

# what about the shadow values (within the variables) themselves?
#
# contains_shadow <- function(x, ...){
#   UseMethod("contains_shadow")
# }
#
# contains_shadow.default <- function(x, ...){
#   all(is.factor(x), grepl("NA", levels(x)))
# }
#
# contains_shadow.data.frame <- function(x, ...){
#   apply(data.frame(x), MARGIN = c(1), contains_shadow.default)
# }

## Example use of this
#
# dfs
#
# library(naniar)
# library(tidyverse)
# dfs <- bind_shadow(df)
#
# dfs %>%
#   mutate(wind_NA = forcats::fct_expand(wind_NA,
#                                        "NA_suffix"),
#          wind_NA = case_when(
#            wind == -99 ~ factor("NA_suffix",
#                                 levels = c("!NA", "NA", "NA_suffix")),
#            TRUE ~ wind_NA
#          ))
#
# df %>%
#   bind_shadow() %>%
#   dplyr::mutate(wind_NA = forcats::as_factor(c("NA_instr",
#                                                "!NA",
#                                                "!NA"))) %>%
#   dplyr::mutate(wind_NA = forcats::fct_expand(wind_NA,
#                                               "!NA","NA","NA_instr"),
#                 wind_NA = forcats::fct_relevel(wind_NA,
#                                                "!NA", "NA", "NA_instr"),
#                 temp_NA = forcats::fct_expand(temp_NA,
#                                               "!NA","NA","NA_instr"),
#                 temp_NA = forcats::fct_relevel(temp_NA,
#                                                "!NA", "NA", "NA_instr"))
#
#
# dfs <- bind_shadow(df)
# dplyr::mutate_if(.tbl = dfs,
#                  .predicate = is_shadow,
#                  .funs = function(x){
#                    # add the factor level
#                    tmp <- forcats::fct_expand(x,"!NA","NA","NA_instr")
#                    # make sure that the order is preserved
#                    forcats::fct_relevel(tmp,"!NA","NA","NA_instr")
#                  }
# )
