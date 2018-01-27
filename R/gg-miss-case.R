#' Plot the number of missings per case (row)
#'
#' This is a visual analogue to `miss_case_summary`. It draws a ggplot of the
#'   number of missings in each case (row). A default minimal theme is used, which
#'   can be customised as normal for ggplot.
#'
#' @param x data.frame
#' @param facet a single bare variable name, if you want to create a faceted plot.
#' @param order_cases logical Order the rows by missingness (default is no - FALSE)
#'
#' @return a ggplot object depicting the number of missings in a given case.
#' @export
#'
#' @examples
#'
#' gg_miss_case(airquality)
#' library(ggplot2)
#' gg_miss_case(airquality) + labs(x = "Number of Cases")
#' gg_miss_case(airquality, order_cases = TRUE)
#' gg_miss_case(airquality, facet = Month)
#' gg_miss_case(airquality, facet = Month, order_cases = TRUE)
#'
gg_miss_case <- function(x, facet, order_cases = FALSE){

  if (!missing(facet)){
    quo_group_by <- rlang::enquo(facet)

    group_string <- deparse(substitute(facet))
  }

  if (order_cases & missing(facet)) {

    ggobject <-
      x %>%
      miss_case_summary(order = TRUE) %>%
      # overwrite case
      dplyr::mutate(case = 1:n()) %>%
      gg_miss_case_create()

  } else if (!order_cases & missing(facet)) {

    ggobject <- x %>%
      miss_case_summary() %>%
      gg_miss_case_create()

  } else if (order_cases & !missing(facet)) {

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      # overwrite case
      miss_case_summary(order = TRUE) %>%
      dplyr::mutate(case = 1:n()) %>%
      gg_miss_case_create() +
      facet_wrap(as.formula(paste("~", group_string)))

  } else if (!order_cases & !missing(facet)) {

    ggobject <- x %>%
      dplyr::group_by(!!quo_group_by) %>%
      miss_case_summary() %>%
      gg_miss_case_create() +
      facet_wrap(as.formula(paste("~", group_string)))

  }

  return(ggobject)

}

# utility function to create the starting block for gg_miss_case ---------------

gg_miss_case_create <- function(data){
  ggplot(data = data,
         aes(x = case,
             y = n_miss)) +
    geom_col(width = 1,
             colour = "grey",
             fill = "grey") +
    coord_flip() +
    labs(y = "# Missing",
         x = "# Cases") +
    theme_minimal() +
    scale_x_reverse()

}
