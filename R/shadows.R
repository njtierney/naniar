#' S3 method for create shadows
#'
#' Shadows have _NA as a suffix
#'
#' @param data dataframe
#' @param ... selected variables to use
#'
#' @return appended shadow with column names
#' @export

as_shadow <- function(data, ...) UseMethod("as_shadow")

#' Create shadow data
#'
#' Return a tibble that in shadow matrix form, where the variables are the same but have a suffix _NA attached to indicate their difference.
#'
#' @inheritParams as_shadow
#'
#' @examples
#'
#' as_shadow(airquality)
#'
#' @export
as_shadow.data.frame <- function(data, ...){

  # if (is.null(vars)){

    data_shadow <- purrr::map_df(data, label_na)

    names(data_shadow) <- paste0(names(data),"_NA")

    data_shadow

    # future dev - try and create a vars argument to only
  # } else {

    # data_shadow <- dplyr::select(data, rlang::.data[[vars]])


    # data_shadow

    # print(data_shadow)

    # %>%  purrr::map_df(label_na)

    # names(data_shadow) <- paste0(names(.data),"_NA")

    # data_shadow

  # }

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

  if(is.data.frame(data) == FALSE){
    stop("Input must be a data.frame", call. = FALSE)
  }

  data_shadow <- as_shadow(data)

  bound_shadow <- dplyr::bind_cols(data, data_shadow)

  tibble::as_tibble(bound_shadow)

}

#' Long form representation of a shadow matrix
#'
#' gather exists as we want to include this extra metadata about the rows that have missing data, but I also wanted to include some extra information about the class of the data, in case we need to gather the data back into a wider, rather than long, format. Here it takes a function `visdat`, `visdat:::fingerprint`, which is currently not a particularly complex function.
#'
#' @param data a dataframe
#'
#' @return a dataframe in long, or "molten" format, containing information about the missings
#' @export
#'
#' @examples
#'
#' gather_shadow(airquality)
#'
gather_shadow <- function(data){

  if(is.data.frame(data) == FALSE){
    stop("Input must be a data.frame", call. = FALSE)
  }

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
#     purrr::map_df(visdat:::fingerprint) %>%
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

