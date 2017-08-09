#' @importFrom visdat vis_miss
#' @export
visdat::vis_miss

#' Plot the number of missings per case (row)
#'
#' This is a visual analogue to `miss_case_summary`. It draws a ggplot of the
#'   number of missings in each case (row). A default minimal theme is used, which
#'   can be customised as normal for ggplot.
#'
#' @param x data.frame
#'
#' @return a ggplot object depicting the number of missings in a given case.
#' @export
#'
#' @examples
#'
#' gg_miss_case(airquality)
#' library(ggplot2)
#' gg_miss_case(airquality) + labs(x = "Number of Cases")
#'
gg_miss_case <- function(x){

  ggobject <- ggplot(data = miss_case_summary(x),
         aes(y = n_missing,
             x = case)) +
    geom_bar(stat = "identity",
             position = "dodge",
             width = 0,
             colour = "grey") +
    geom_point() +
    coord_flip() +
    labs(y = "# Missing",
         x = "Cases") +
    theme_minimal()

  return(ggobject)

}

#' Plot the number of missings for each variable
#'
#' This is a visual analogue to `miss_var_summary`. It draws a ggplot of the
#'   number of missings in each variable, ordered to show which variables have
#'   the most missing data. A default minimal theme is used, which can be
#'   customised as normal for ggplot.
#'
#' @param x a dataframe
#' @param show_pct logical shows the number of missings (default), but if set to
#'  TRUE, it will display the proportion of missings.
#'
#' @return a ggplot object depicting the number of missings in a given column
#' @export
#'
#' @examples
#'
#' gg_miss_var(airquality)
#' library(ggplot2)
#' gg_miss_var(airquality) + labs(y = "Look at all the missing ones")
#'
gg_miss_var <- function(x, show_pct = FALSE){

  # get a tidy data frame of the number of missings in each column
  test_if_dataframe(x)
  test_if_null(x)

  if (show_pct == FALSE) {

  ggobject <- x %>%
    miss_var_summary() %>%
    ggplot(data = .,
           aes(x = stats::reorder(variable, n_missing),
               y = n_missing,
               colour = variable)) +
    geom_bar(stat = "identity",
             position = "dodge",
             width = 0) +
    geom_point() +
    coord_flip() +
    scale_color_discrete(guide = FALSE) +
    labs(y = "# Missing",
         x = "Variables") +
    theme_minimal()

  } else if (show_pct == TRUE) {

    ggobject <- x %>%
      miss_var_summary() %>%
      ggplot(data = .,
             aes(x = stats::reorder(variable, percent),
                 y = percent,
                 colour = variable)) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0) +
      geom_point() +
      coord_flip() +
      scale_color_discrete(guide = FALSE) +
      labs(y = "Proportion Missing",
           x = "Variables") +
      theme_minimal()

  }

  return(ggobject)

}

#' Plot which variables contain a missing value
#'
#' This plot produces a set of rectangles indicating whether there is a missing
#'   element in a column or not.  A default minimal theme is used, which can be
#'   customised as normal for ggplot.
#'
#' @param x a dataframe
#'
#' @return a ggplot object
#'
#' @export
#'
#' @examples
#'
#' gg_miss_which(airquality)
#' library(ggplot2)
#'

gg_miss_which <- function(x){

  # tell us which columns have missing data
  ggobject <- x %>%
    purrr::map_df(anyNA) %>%
    purrr::map_df(function(x) ifelse(x == 0, "complete", "missing")) %>%
    tidyr::gather(key = "variable",
                  value = "value") %>%
    dplyr::mutate(nheight = 1) %>%
    ggplot(data = .,
           aes(x = variable,
               y = nheight,
               fill = factor(value))) +
    geom_tile(colour = "white") +
    theme_minimal() +
    scale_fill_grey(name = "") +
    scale_x_discrete(limits = names(x)) +
    theme(legend.position = "none") +
    scale_y_discrete(breaks = c(""),
                     labels = c("")) +
    labs(y = " ",
         x = " ")

  return(ggobject)

}

#' Plot the number of missings for each variable, broken down by a factor
#'
#' This function draws a ggplot plot of the number of missings in each column,
#'   broken down by a categorical variable from the dataset. A default minimal
#'   theme is used, which can be customised as normal for ggplot.
#'
#' @param x data.frame
#' @param fct column containing the factor variable
#'
#' @return ggplot object depicting the number of missings
#' @export
#'
#' @examples
#'
#' gg_miss_fct(x = riskfactors, fct = marital)
#' library(ggplot2)
#' gg_miss_fct(x = riskfactors, fct = marital) + labs(title = "NA in Risk Factors and Marital status")
#'

gg_miss_fct <- function(x, fct){

  fct <- rlang::enquo(fct)

  ggobject <- x %>%
    dplyr::group_by(!!fct) %>%
    miss_var_summary() %>%
    ggplot(aes_string(quo_name(fct),
               "variable",
               fill = "percent")) +
    geom_tile() +
    viridis::scale_fill_viridis()

  return(ggobject)
}

#' Plot of cumulative sum of missing value for each variable
#'
#' A plot showing the cumulative sum of missing values for each variable,
#' reading columns from the left to the right of the initial dataframe. A
#' default minimal theme is used, which can be customised as normal for ggplot.
#'
#' @param x a data.frame
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' gg_miss_var_cumsum(airquality)

gg_miss_var_cumsum <- function(x){

  ggobject <- x %>%
    miss_var_cumsum() %>%
    ggplot(aes(x = stats::reorder(variable, n_missing_cumsum),
               y = n_missing_cumsum,
               group = 1)) +
    geom_line(size = 2) +
    labs(x = "Var",
         y = "Cumsum of missing values") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(ggobject)
}

#' Plot of cumulative sum of missing for cases
#'
#' A plot showing the cumulative sum of missing values for cases, reading the
#' rows from the top to bottom. A default minimal theme is used, which can be
#' customised as normal for ggplot.
#'
#' @param x a dataframe
#' @param breaks the breaks for the x axis default is 20
#'
#' @return a ggplot object depicting the number of missings
#' @export
#'
#' @examples
#'
#' gg_miss_case_cumsum(airquality)
#' library(ggplot2)
#' gg_miss_case_cumsum(riskfactors, breaks = 50) + theme_bw()

gg_miss_case_cumsum <- function(x, breaks = 20){

  ggobject <- x %>%
    miss_case_cumsum() %>%
    ggplot(aes(x = stats::reorder(case, n_missing_cumsum),
               y = n_missing_cumsum,
               group = 1)) +
    geom_line(size = 2) +
    labs(x = "Case",
         y = "Cumsum of missing values") +
    scale_x_discrete(breaks = seq(0,
                                  nrow(x),
                                  by = breaks)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(ggobject)

}

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
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#'
#' miss_var_span(pedestrian, hourly_counts, span_every = 3000)
#' library(ggplot2)
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000)
#' # works with the rest of ggplot
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000) + labs(x = "custom")
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000) + theme_dark()

gg_miss_span <- function(data,
                         var,
                         span_every){

  var_enquo <- rlang::enquo(var)

  ggobject <-  miss_var_span(data = data,
                             var = !!var_enquo,
                             span_every = span_every) %>%
    # miss_var_span(pedestrian, hourly_counts, span_every = 3000) %>%
    tidyr::gather(key = variable,
                  value = value,
                  prop_miss:prop_complete) %>%
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


