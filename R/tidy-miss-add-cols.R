#' Add a counter variable for a span of dataframe
#'
#' Adds a variable, `span_counter` to a dataframe. Used internally to facilitate
#' counting of missing values over a given span.
#'
#' @param data data.frame
#' @param span_size integer
#'
#' @return data.frame with extra variable "span_counter".
#'
#' @examples
#' \dontrun{
#' add_span_counter(pedestrian, span_size = 100)
#' }
add_span_counter <- function(data, span_size) {

  dplyr::mutate(data,
                span_counter = rep(x = 1:ceiling(nrow(data)),
                                   each = span_size,
                                   length.out = nrow(data)))
}

#' Add column containing number of missing data values
#'
#' It can be useful when doing data analysis to add the number of missing data points into your dataframe. add_n_miss adds a column named "n_miss", which contains the number of missing values in that row.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' airquality %>% add_n_miss()
#'
#'
add_n_miss <- function(data){

  purrrlyr::by_row(.d = data,
                ..f = function(x) n_miss(x),
                .collate = "row",
                .to = "n_miss")

  # old approach
  # # create a numeric vector of n_missing
  # col_n_miss <- data.frame(n_miss = apply(df,1,n_miss))
  #
  # dplyr::bind_cols(df,col_n_miss)

}

#' Add column containing proportion of missing data values
#'
#' It can be useful when doing data analysis to add the proportion of missing data values into your dataframe. add_prop_miss adds a column named "prop_miss", which contains the proportion of missing values in that row.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' airquality %>% add_prop_miss()
#'
#' # this can be applied to model the proportion of missing data
#' # as in Tierney et al bmjopen.bmj.com/content/5/6/e007450.full
#' library(rpart)
#' library(rpart.plot)
#'
#' airquality %>%
#' add_prop_miss() %>%
#' rpart(prop_miss ~ ., data = .) %>%
#' prp(type = 4,
#'     extra = 101,
#'     prefix = "prop_miss = ")


add_prop_miss <- function(data){

  purrrlyr::by_row(.d = data,
                ..f = function(x) (mean(is.na(x))),
                .collate = "row",
                .to = "prop_miss")

  # old approach
  # df %>%
  #   add_n_miss() %>%
  #   dplyr::mutate(pct_miss = n_miss/ncol(df)) %>%
  #   dplyr::select(-n_miss)

}

# purrr::by_row may be deprecated soon, keeping old methods below
# just as a fallback


#' Add a shadow shifted column to a dataset
#'
#' Shifting the values of a numeric
#'
#' @param data data.frame or .tbl
#' @param vars quoted variables
#' @param suffix suffix to add to variable, defaults to "shift"
#'
#' @return .data with the added variable shifted named as `var_suffix`
#'
#' @export
#'
#' @examples
#'
#' add_shadow_shift(pedestrian, vars = "hourly_counts")
#' add_shadow_shift(airquality, vars = c("Ozone", "Solar.R"))
#'
add_shadow_shift <- function(data, vars, suffix = "shift"){

  # select variables
    quo_vars <- rlang::quos(vars)

    shadow_shifted_vars <- dplyr::select(data, !!!quo_vars)

  # shadow shift all (using purrr:map_df)
    # would be good to have a way of indicating that no shift was taken at all
    shadow_shifted_df <- purrr::map_df(shadow_shifted_vars, shadow_shift)

  # change names
  names(shadow_shifted_df) <- paste0(names(shadow_shifted_df),"_",suffix)

  tibble::as_tibble(dplyr::bind_cols(data, shadow_shifted_df))

}

#' Add a shadow column to a dataset
#'
#' Shifting the values of a numeric
#'
#' @param data data.frame or .tbl
#' @param vars quoted variablename
#'
#' @return .data with the added variable shifted named as `var_NA`
#'
#' @export
#'
#' @examples
#'
cast_shadow <- function(data, vars){

  quo_vars <- rlang::quos(vars)

  # shadow all (using purrr:map_df)
  shadow_vars <- dplyr::select(data, !!!quo_vars) %>% as_shadow
  # cannot get this to take multiple variables
  # shadow_vars <- dplyr::select(data, .data[[vars]]) %>% as_shadow

  tibble::as_tibble(dplyr::bind_cols(data, shadow_vars))

}
