#' Plot the number of missings for each variable
#'
#' This is a visual analogue to `miss_var_summary`. It draws a ggplot of the
#'   number of missings in each variable, ordered to show which variables have
#'   the most missing data. A default minimal theme is used, which can be
#'   customised as normal for ggplot.
#'
#' @param x a dataframe
#' @param group bare variable name, if you want to create a faceted plot.
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
#' gg_miss_var(airquality, Month)
#' gg_miss_var(airquality, Month, TRUE)
#'
gg_miss_var <- function(x, group, show_pct = FALSE){

  # get a tidy data frame of the number of missings in each column
  test_if_dataframe(x)
  test_if_null(x)

  # collect group into
  quo_group_by <- rlang::enquo(group)
  group_string <- deparse(substitute(group))

  if (show_pct == FALSE & missing(group)) {

    ggobject <- x %>%
      miss_var_summary() %>%
      ggplot(data = .,
             aes(x = stats::reorder(variable, n_miss),
                 y = n_miss,
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

  } else if (show_pct == TRUE & missing(group)) {

    ggobject <- x %>%
      miss_var_summary() %>%
      ggplot(data = .,
             aes(x = stats::reorder(variable, pct_miss),
                 y = pct_miss,
                 colour = variable)) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0) +
      geom_point() +
      coord_flip() +
      scale_color_discrete(guide = FALSE) +
      labs(y = "% Missing Missing",
           x = "Variables") +
      theme_minimal()

    # show the groupings -------------------------------------------------------

  } else if (show_pct == FALSE & !missing(group)){

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      miss_var_summary() %>%
      ggplot(data = .,
             aes(x = stats::reorder(variable, n_miss),
                 y = n_miss,
                 colour = variable)) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0) +
      geom_point() +
      facet_wrap(as.formula(paste("~", group_string))) +
      coord_flip() +
      scale_color_discrete(guide = FALSE) +
      labs(y = "# Missing",
           x = "Variables") +
      theme_minimal()

  } else if (show_pct == TRUE & !missing(group)){

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      miss_var_summary() %>%
      ggplot(data = .,
             aes(x = stats::reorder(variable, n_miss),
                 y = pct_miss,
                 colour = variable)) +
      geom_bar(stat = "identity",
               position = "dodge",
               width = 0) +
      geom_point() +
      facet_wrap(as.formula(paste("~", group_string))) +
      coord_flip() +
      scale_color_discrete(guide = FALSE) +
      labs(y = "% Missing",
           x = "Variables") +
      theme_minimal()

  }

  return(ggobject)

}
