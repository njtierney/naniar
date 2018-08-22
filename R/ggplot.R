#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

new_mc_sum <- function(x, ...){
  tibble::new_tibble(x, subclass = "mc_sum")
}

#' Plot miss case summary
#'
#' @param object object run by miss_case_summary
#' @param ... additional arguments
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' miss_case_summary(airquality) %>% autoplot()
autoplot.mc_sum <- function(object, ...){

  # defensive code to make sure people have ggplot2 installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  }

  # check that the object is what you expect
  else if (!inherits(object, "mc_sum")) {
      stop("autoplot.mc_sum requires an mc_sum object, use object=object")
  }

  # plotting code
    ggplot2::ggplot(object,
                    ggplot2::aes(x = case,
                                 y = n_miss)) +
      ggplot2::geom_col(width = 1,
                        colour = "#484878", # lorikeet purple
                        fill = "#484878") + # lorikeet purple
      ggplot2::coord_flip() +
      ggplot2::labs(y = "# Missing",
                    x = "Cases") +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_reverse()

}
