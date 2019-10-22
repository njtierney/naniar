#' Plot the pattern of missingness using an upset plot.
#'
#' Upset plots are a way of visualising common sets, `gg_miss_upset`` shows the
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



