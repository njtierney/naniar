#' Create shadows
#'
#' Representing missing data structure is achieved using the shadow matrix,
#' introduced in [Swayne and Buja](https://www.researchgate.net/publication/2758672_Missing_Data_in_Interactive_High-Dimensional_Data_Visualization). The shadow
#' matrix is the same dimension as the data, and consists of binary indicators
#' of missingness of data values, where missing is represented as "NA", and not
#' missing is represented as "!NA". Although these may be represented as 1 and
#' 0, respectively. This representation can be seen in the figure below, adding
#' the suffix "_NA" to the variables.
#'
#' @param data dataframe
#' @param ... selected variables to use
#'
#' @return appended shadow with column names
#' @export

as_shadow <- function(data, ...){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("as_shadow")
}

#' Create shadow data
#'
#' Return a tibble in shadow matrix form, where the variables are the same but
#' have a suffix _NA attached to distinguish them.
#'
#' @inheritParams as_shadow
#'
#' @examples
#'
#' as_shadow(airquality)
#'
#' @export
as_shadow.data.frame <- function(data, ...){

    data_shadow <- purrr::map_df(data, label_shadow_matrix)

    names(data_shadow) <- paste0(names(data),"_NA")

    data_shadow

}

#' Bind a shadow dataframe to original data
#'
#' Binding a shadow matrix to a regular dataframe helps visualise and work with
#' missing data.
#'
#' @param data a dataframe
#'
#' @return dataframe with an extra appended layer of data.
#' @export
#'
#' @examples
#'
#' aq_shadow <- bind_shadow(airquality)
#'
#' # explore missing data visually
#' library(ggplot2)
#'
#' # using the bounded shadow
#'
#' ggplot(data = aq_shadow,
#'        aes(x = Ozone)) +
#'        geom_histogram() +
#'        facet_wrap(~Solar.R_NA,
#'        ncol = 1)
#'
bind_shadow <- function(data){

  data_shadow <- as_shadow(data)

  bound_shadow <- dplyr::bind_cols(data, data_shadow)

  tibble::as_tibble(bound_shadow)

}

#' Long form representation of a shadow matrix
#'
#' `gather_shadow` is a long-form representation of binding the shadow matrix to
#'   your data, producing variables named `rows`, `var`, and `miss`, where
#'   `miss` contains the missing value representation.
#'
#' @param data a dataframe
#'
#' @return dataframe in long, format, containing information about the missings
#'
#' @export
#'
#' @examples
#'
#' gather_shadow(airquality)
#'
gather_shadow <- function(data){

  as_shadow(data) %>%
    dplyr::mutate(rows = 1:nrow(.)) %>%
    tidyr::gather(key = "var",
                  value = "miss",
                  -rows)
}
