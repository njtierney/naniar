#' Plot the number of missings for each variable
#'
#' This is a visual analogue to `miss_var_summary`. It draws a ggplot of the
#'   number of missings in each variable, ordered to show which variables have
#'   the most missing data. A default minimal theme is used, which can be
#'   customised as normal for ggplot.
#'
#' @param x a dataframe
#' @param facet (optional) bare variable name, if you want to create a faceted plot.
#' @param show_pct logical shows the number of missings (default), but if set to
#'  TRUE, it will display the proportion of missings.
#'
#' @return a ggplot object depicting the number of missings in a given column
#'
#' @seealso [geom_miss_point()] [gg_miss_case()] [gg_miss_case_cumsum] [gg_miss_fct()] [gg_miss_span()] [gg_miss_var()] [gg_miss_var_cumsum()] [gg_miss_which()]
#'
#' @export
#'
#' @examples
#'
#' gg_miss_var(airquality)
#' library(ggplot2)
#' gg_miss_var(airquality) + labs(y = "Look at all the missing ones")
#' gg_miss_var(airquality, Month)
#' gg_miss_var(airquality, Month, show_pct = TRUE)
#' gg_miss_var(airquality, Month, show_pct = TRUE) + ylim(0, 100)
#'
gg_miss_var <- function(x, facet, show_pct = FALSE){

  # get a tidy data frame of the number of missings in each column
  test_if_dataframe(x)
  test_if_null(x)

  if (!missing(facet)) {
  # collect group into
  quo_group_by <- rlang::enquo(facet)
  group_string <- deparse(substitute(facet))
  }

  if (missing(facet)) {

    ggobject <- x %>%
      miss_var_summary() %>%
      gg_miss_var_create(show_pct = show_pct)

    return(ggobject)

    # show the groupings -------------------------------------------------------

  } else if (!missing(facet)) {

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      miss_var_summary() %>%
      gg_miss_var_create(show_pct = show_pct) +
      facet_wrap(as.formula(paste("~", group_string)))


  return(ggobject)

}

}
# utility function to create the starting block for gg_miss_var ---------------

gg_miss_var_create <- function(data, show_pct){

  if (show_pct) {
    ylab <- "% Missing"
    aes_y <- "pct_miss"
  } else if (!show_pct){
    ylab <- "# Missing"
    aes_y <- "n_miss"
  }


  ggplot(data = data,
       aes(x = stats::reorder(variable, n_miss))) +
       #     y = n_miss)) +
       # aes(x = stats::reorder(variable, n_miss),
       #     y = n_miss)) +
  geom_bar(aes_string(y = aes_y),
           stat = "identity",
           position = "dodge",
           width = 0,
           colour = "#484878",
           fill = "#484878") +
  geom_point(aes_string(y = aes_y),
             colour = "#484878",
             fill = "#484878") +
  coord_flip() +
  scale_color_discrete(guide = FALSE) +
  labs(y = ylab,
       x = "Variables") +
  theme_minimal()

}

