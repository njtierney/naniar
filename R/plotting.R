# plotting functions for narnia


#' @importFrom visdat vis_miss
#' @export
visdat::vis_miss

#' Plot the number of missings per case (row)
#'
#' This function draws a ggplot of the number of missings in each row.
#' At this point I'm not sure how this plot should be arranged, as it currently looks a bit ugly!
#'
#' @param x a dataframe
#' @param xlab legend for the x axis. Default is "# Missing".
#' @param ylab legend for the y axis. Default is "Cases".
#' @param title title for the plot. Default is NULL.
#' @param ... you can pass more ggplot theme parameters inside the ...
#'
#' @return a ggplot plot depicting the number of missings in a given case
#' @export
#'
#' @examples
#'
#' gg_miss_case(airquality)
#' gg_miss_case(airquality, title = "Missing cases")
#'
gg_miss_case <- function(x, xlab = "Cases", ylab = "# Missing", title = NULL, ...){

  ggplot(data = miss_case_summary(x),
         aes(y = n_missing,
             x = case)) +
    geom_bar(stat="identity", position="dodge", width = 0, colour="grey") +
    geom_point() +
    coord_flip() +
    labs(y = ylab,
         x = xlab,
         title = title) +
    theme_minimal() +
    theme(...)

}

#' Plot the number of missings for each variable
#'
#' This function draws a ggplot plot of the number of missings in each column, rearranged to show which variables have the most missing data.
#'
#' @param x a dataframe
#' @param xlab legend for the x axis. Default is "# Missing".
#' @param ylab legend for the y axis. Default is "Variables".
#' @param title title for the plot. Default is NULL.
#' @param ... you can pass more ggplot theme parameters inside the ...
#'
#' @return a ggplot plot depicting the number of missings in a given column
#' @export
#'
#' @examples
#'
#' gg_miss_var(airquality)
#' gg_miss_var(airquality, ylab = "Number of missing values")
#' gg_miss_var(airquality, title = "Missing values in airquality")
#' library(ggplot2)
#' gg_miss_var(airquality, panel.background = element_rect(fill = "grey"))
#'
#'
gg_miss_var <- function(x, xlab = "Variables", ylab = "# Missing", title = NULL, ...){

  # get a tidy data frame of the number of missings in each column
  x %>%
    miss_var_summary() %>%
    ggplot(data = .,
           aes(x = stats::reorder(variable, n_missing),
               y = n_missing,
               colour = variable)) +
    geom_bar(stat="identity", position="dodge", width = 0) +
    geom_point() +
    coord_flip() +
    labs(y = ylab,
         x = xlab,
         title = title) +
    theme_minimal() +
    theme(legend.position = "none", ...)

}

#' Plot which variables contain a missing value
#'
#' `gg_miss_which` (need a better name!) produces a set of rectangles that indicate whether there is a missing element in a column or not
#'
#' @param x a dataframe
#' @param xlab legend for the x axis. Default is " ".
#' @param ylab legend for the y axis. Default is " ".
#' @param title title for the plot. Default is NULL.
#' @param ... you can pass more ggplot theme parameters inside the ...
#'
#' @return a ggplot plot
#'
#' @export
#'
#' @examples
#'
#' gg_miss_which(airquality)
#' gg_miss_which(airquality, xlab = "Variables", ylab = "Height")
#' gg_miss_which(airquality, title = "Missing values in airquality")
#' library(ggplot2)
#' gg_miss_which(airquality, panel.background = element_rect(fill = "grey"))

gg_miss_which <- function(x, xlab = " ", ylab = " ", title = NULL, ...){

  # tell us which columns have missing data
  # airquality %>%
  x %>%
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
    scale_y_discrete(breaks=c(""),
                     labels=c("")) +
    labs(y = ylab,
         x = xlab,
         title = title) +
    theme(...)
}


#' Plot the number of missings in a given repeating span
#'
#' This is a replacement function to
#' imputeTS::plotNA.distributionBar(tsNH4, breaksize = 100), which shows the
#' number of missings in a given span
#'
#' @param data data.frame
#' @param var a bare unquoted variable name
#' @param span_size integer describing the length of the span to be explored
#' @param xlab legend for the x axis. Default is "Span".
#' @param ylab legend for the y axis. Default is "Proportion Missing".
#' @param title title for the plot. Default is "Proportion of missing values".
#' @param subtitle subtitle for the plot. Default paste "Over a repeating span of" and span_size.
#' @param ... you can pass more ggplot theme parameters inside the ...
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#'
#'\dontrun{
#' miss_var_span(pedestrian, hourly_counts, span_every = 3000)
#' gg_miss_span(pedestrian, hourly_counts, span_every = 3000)
#' }

gg_miss_span <- function(data,
                         var,
                         span_size,
                         title = "Proportion of missing values",
                         subtitle = sprintf("Over a repeating span of %s", span_size),
                         xlab = "Span",
                         ylab = "Proportion Missing",
                         ...){

  miss_var_span(data = data,
                var = var,
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
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  x = xlab,
                  y = ylab) +
    ggplot2::theme(...)

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
