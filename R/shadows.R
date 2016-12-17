# A set of functions that provide utility functions for creating "shadow" dataframes (shadaframes, nataframes, nabbles)
#'
#'
#' return the number of missing values
#'
#' substitute for length(is.na())
#'
#' @param x a vector
#'
#' @export
#'
n_miss <- function(x){
  length(is.na(x))
}

#' Give NAs a more meaningful label
#'
#' Returns a binary factor of !NA and NA, where !NA indicates a datum that is not missing, and NA indicats missingness.
#'
#' @param x a vector
#'
#' @return a vector
#' @export
#'
#' @seealso as_shadow
#'
#' @examples
#'
#' is_na(airquality$Ozone)
#'
is_na <- function(x) {
  factor(is.na(x),
         levels = c(FALSE, TRUE),
         labels = c("!NA", "NA"))
}


#' Create shadow data
#'
#' Return a tibble that in shadow matrix form, where the variables are the same but have a suffix _NA attached to indicate their difference.
#'
#' @param data a dataframe
#'
#' @return a dataframe with appended
#' @export
#'
#' @examples
#'
#' as_shadow(airquality)
#'
as_shadow <- function(data){

  data_shadow <- purrr::map_df(data, is_na)

  names(data_shadow) <- paste0(names(data),"_NA")

  data_shadow

}

#' Column bind a shadow dataframe to original data
#'
#' Binding a shadow dataframe to a regular dataframe helps visualise and work with missing data
#'
#' @param data a dataframe
#'
#' @return dataframe with an extra appended layer of data.
#' @export
#'
#' @examples
#'
#' bind_shadow(airquality)
#'
#' # explore missing data visually
#' library(ggplot2)
#'
#' # using the bounded shadow
#'
#' ggplot(data = bind_shadow(airquality),
#'        aes(x = Ozone)) +
#'        geom_histogram() +
#'        facet_wrap(~Solar.R_NA,
#'        ncol = 1)
#'
#' # using the is_na function
#'
#' ggplot(data = airquality,
#'        aes(x = Ozone)) +
#'   geom_histogram() +
#'   facet_wrap(~is_na(Solar.R),
#'              ncol = 1)
#'
bind_shadow <- function(data){

  data_shadow <- as_shadow(data)

  bound_shadow <- dplyr::bind_cols(data, data_shadow)

  tibble::as_tibble(bound_shadow)

}

#' gather_shadow
#'
#' gather exists as we want to include this extra metadata about the rows that have missing data, but I also wanted to include some extra information about the class of the data, in case we need to gather the data back into a wider, rather than long, format. Here it takes a function `visdat`, `visdat:::fingerprint`, which is currently not a particularly complex function.
#'
#' @param data a dataframe
#'
#' @return a dataframe in long, or  "molten" format, containing information about the missings
#' @export
#'
#' @examples
#'
#' aq_shadow <- gather_shadow(airquality)
#'
#' aq_shadow

#'
gather_shadow <- function(data){

  as_shadow(data) %>%
    dplyr::mutate(rows = 1:nrow(.)) %>%
    tidyr::gather(key = "var",
           value = "miss",
           -rows)
}

# here is a different version of gather_shadow which preserves value
# type information about gather
# gather_shadow <- function(df){
#
#   df_val_type <- df %>%
#     tibble::as_tibble() %>%
#     purrr::dmap(visdat:::fingerprint) %>%
#     dplyr::mutate(rows = 1:nrow(df)) %>%
#     tidyr::gather_(key_col = "variable",
#                    value_col = "valueType",
#                    gather_cols = names(.)[-length(.)])
#
#   # df_shadow
#   df_shadow <- df %>%
#     tibble::as_tibble() %>%
#     dplyr::mutate(rows = 1:nrow(df)) %>%
#     tidyr::gather(key = variable,
#                   value = value,
#                   -rows) %>%
#     dplyr::mutate(shadow_matrix = is_na(value)) %>%
#     dplyr::left_join(df_val_type)
#
#   return(df_shadow)
#
#   # perhaps define some attributes
#
# }

