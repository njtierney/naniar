#' Plot the number of missings in a given repeating span
#'
#' `gg_miss_span` is a replacement function to
#'   `imputeTS::plotNA.distributionBar(tsNH4, breaksize = 100)`, which shows the
#'   number of missings in a given span, or breaksize. A default minimal theme
#'   is used, which can be customised as normal for ggplot.
#'
#' @param data data.frame
#' @param var a bare unquoted variable name from `data`.
#' @param span_every integer describing the length of the span to be explored
#' @param facet (optional) a single bare variable name, if you want to create a faceted plot.
#'
#' @seealso [geom_miss_point()] [gg_miss_case()] [gg_miss_case_cumsum()] [gg_miss_fct()] [gg_miss_var()] [gg_miss_var_cumsum()] [gg_miss_which()]

#' @return ggplot2 showing the number of missings in a span (window, or breaksize)
#' @export
#'
#' @examples
#'
#' miss_var_span(pedestrian, hourly_counts, span_every = 3000)
#' \dontrun{
#' library(ggplot2)
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000)
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000, facet = sensor_name)
#' # works with the rest of ggplot
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000) + labs(x = "custom")
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000) + theme_dark()
#' }

gg_miss_span <- function(data,
                         var,
                         span_every,
                         facet){

  var_enquo <- rlang::enquo(var)

  if (missing(facet)) {

  ggobject <-  miss_var_span(data = data,
                             var = !!var_enquo,
                             span_every = span_every) %>%
    tidyr::pivot_longer(cols = prop_miss:prop_complete,
                        names_to = "variable",
                        values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = span_counter,
                                 y = value,
                                 fill = variable)) +
    ggplot2::geom_col(colour = "white") +
    ggplot2::scale_fill_manual(name = "",
                               values = c("grey80",
                                          "grey20"),
                               label = c("Present",
                                         "Missing")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Proportion of missing values",
                  subtitle = sprintf("Over a repeating span of %s", span_every),
                  x = "Span",
                  y = "Proportion Missing")

  }

  if (!missing(facet)){

    quo_group_by <- rlang::enquo(facet)

    group_string <- deparse(substitute(facet))

    ggobject <-
    data %>%
      dplyr::group_by(!!quo_group_by) %>%
      miss_var_span(var = !!var_enquo,
                    span_every = span_every) %>%
      tidyr::pivot_longer(cols = prop_miss:prop_complete,
                          names_to = "variable",
                          values_to = "value") %>%
      ggplot2::ggplot(ggplot2::aes(x = span_counter,
                                   y = value,
                                   fill = variable)) +
      ggplot2::geom_col(colour = "white") +
      ggplot2::scale_fill_manual(name = "",
                                 values = c("grey80",
                                            "grey20"),
                                 label = c("Present",
                                           "Missing")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Proportion of missing values",
                    subtitle = sprintf("Over a repeating span of %s",
                                       span_every),
                    x = "Span",
                    y = "Proportion Missing") +
      facet_wrap(as.formula(paste("~", group_string)))

  }

  return(ggobject)

}

# possible alternative plot for missings over a span, using loess to control
# the smoothing of the missingness
#
# tsNH4_NA %>%
#   ggplot(aes(x = date_time,
#              y = x_NA)) +
#   geom_point()
#
# loess_NA <- loess(as.numeric(x_NA) ~ as.numeric(date_time),
#                   data = tsNH4_NA,
#                   degree = 0,
#                   span = 0.75)
#
# modelr::add_predictions(data = tsNH4_NA,
#                         model = loess_NA) %>%
#   mutate(pred = pred - 1) %>%
#   ggplot(aes(x = date_time,
#              y = pred)) +
#   geom_line() +
#   ylim(0,1)


