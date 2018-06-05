#' Plot the number of missings per case (row)
#'
#' This is a visual analogue to `miss_case_summary`. It draws a ggplot of the
#'   number of missings in each case (row). A default minimal theme is used, which
#'   can be customised as normal for ggplot.
#'
#' @param x data.frame
#' @param facet (optional) a single bare variable name, if you want to create a faceted plot.
#' @param order_cases logical Order the rows by missingness (default is FALSE -
#'    no order).
#' @param show_pct logical Show the percentage of cases
#'
#' @return a ggplot object depicting the number of missings in a given case.
#'
#' @seealso [geom_miss_point()] [gg_miss_case_cumsum] [gg_miss_fct()] [gg_miss_span()] [gg_miss_var()] [gg_miss_var_cumsum()] [gg_miss_which()]
#'
#' @export
#'
#' @examples
#'
#' gg_miss_case(airquality)
#' library(ggplot2)
#' gg_miss_case(airquality) + labs(x = "Number of Cases")
#' gg_miss_case(airquality, show_pct = TRUE)
#' gg_miss_case(airquality, order_cases = FALSE)
#' gg_miss_case(airquality, facet = Month)
#' gg_miss_case(airquality, facet = Month, order_cases = FALSE)
#' gg_miss_case(airquality, facet = Month, show_pct = TRUE)
#'
gg_miss_case <- function(x, facet, order_cases = TRUE, show_pct = FALSE){

  if (!missing(facet)) {
    quo_group_by <- rlang::enquo(facet)

    group_string <- deparse(substitute(facet))
  }

  if (order_cases & missing(facet)) {

    ggobject <-
      x %>%
      miss_case_summary(order = TRUE) %>%
      # overwrite case
      dplyr::mutate(case = 1:n()) %>%
      gg_miss_case_create(show_pct = show_pct)

  } else if (!order_cases & missing(facet)) {

    ggobject <- x %>%
      miss_case_summary() %>%
      gg_miss_case_create(show_pct = show_pct)

  } else if (order_cases & !missing(facet)) {

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      # overwrite case
      miss_case_summary(order = TRUE) %>%
      dplyr::mutate(case = 1:n()) %>%
      gg_miss_case_create(show_pct = show_pct) +
      facet_wrap(as.formula(paste("~", group_string)))

  } else if (!order_cases & !missing(facet)) {

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      miss_case_summary() %>%
      gg_miss_case_create(show_pct = show_pct) +
      facet_wrap(as.formula(paste("~", group_string)))

  }

  return(ggobject)

}

# utility function to create the starting block for gg_miss_case ---------------

gg_miss_case_create <- function(data, show_pct){

  if (show_pct) {
   ylab <- "% Missing"
   aes_y <- "pct_miss"
  } else if (!show_pct){
    ylab <- "# Missing"
    aes_y <- "n_miss"
  }

  ggplot(data = data,
         aes_string(x = "case",
             # possibly include an if() statement here to change `n_miss` to
             # `pct_miss` when the appropriate indicator is passed through
             y = aes_y)) +
    geom_col(width = 1,
             colour = "#484878", # lorikeet purple
             fill = "#484878") + # lorikeet purple
    coord_flip() +
    labs(y = ylab,
         x = "Cases") +
    theme_minimal() +
    scale_x_reverse()

}
