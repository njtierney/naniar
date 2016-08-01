#' ggmissing
#'
#' @name ggmissing
#' @docType package
#' @importFrom tidyr gather
#' @importFrom purrr by_row
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom purrr dmap
#' @importFrom ggalt geom_lollipop
#' @importFrom ggalt GeomLollipop
#' @import tibble
#' @import dplyr
#' @import ggplot2
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
