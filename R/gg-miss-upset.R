#' Plot the pattern of missingness using an upset plot.
#'
#' Upset plots are a way of visualising common sets, `gg_miss_upset` shows the
#'   number of missing values for each of the sets of data. The default option
#'   of `gg_miss_upset` is taken from `UpSetR::upset` - which is to use up to 5
#'   sets and up to 40 interactions. We also set the ordering to by the
#'   frequency of the intersections. Setting `nsets = 5` means to look at 5
#'   variables and their combinations. The number of combinations or rather
#'   `intersections` is controlled by `nintersects`. If there are 40
#'   intersections, there will be 40 combinations of variables explored. The
#'   number of sets and intersections can be changed by passing arguments `nsets
#'   = 10` to look at 10 sets of variables, and `nintersects = 50` to look at 50
#'   intersections.
#'
#' @param data data.frame
#' @param order.by (from UpSetR::upset) How the intersections in the matrix should be ordered by. Options include frequency (entered as "freq"), degree, or both in any order.  See `?UpSetR::upset` for more options
#' @param ... arguments to pass to upset plot - see `?UpSetR::upset`
#'
#' @return a ggplot visualisation of missing data
#'
#' @examples
#'
#' \dontrun{
#' gg_miss_upset(airquality)
#' gg_miss_upset(riskfactors)
#' gg_miss_upset(riskfactors, nsets = 10)
#' gg_miss_upset(riskfactors, nsets = 10, nintersects = 10)
#' }
#' @export
gg_miss_upset <- function(data, order.by = "freq", ...){

  data %>%
    as_shadow_upset() %>%
    UpSetR::upset(order.by = order.by,
                  ...)

}



#' Convert data into shadow format for doing an upset plot
#'
#' Upset plots are a way of visualising common sets, this function transforms
#'     the data into a format that feeds directly into an upset plot
#'
#' @param data a data.frame
#'
#' @return a data.frame
#'
#' @examples
#'
#' \dontrun{
#'
#' library(UpSetR)
#' airquality %>%
#'   as_shadow_upset() %>%
#'   upset()
#' }
#'
#' @export
as_shadow_upset <- function(data){

  if (n_var_miss(data) <= 1 ) {

    if (n_var_miss(data) == 1) {
      glu_st <- glue::glue("upset plots for missing data requre at least two \\
                         variables to have missing data, only one variable, \\
                         '{miss_var_which(data)}' has missing values.")
    }

    if (n_var_miss(data) == 0) {

      glu_st <- glue::glue("upset plots for missing data requre at least two \\
                         variables to have missing data, there are no missing \\
                         values in your data! This is probably a good thing.")
    }

    rlang::abort(message = glu_st)
  }

  test_if_null(data)

  test_if_dataframe(data)

  data_shadow <- as.data.frame(is.na(data)*1)

  names(data_shadow) <- paste0(names(data),"_NA")

  dplyr::mutate_if(data_shadow, is.numeric, as.integer)

}
