#' @importFrom rlang quos
#' @importFrom dplyr select_vars
select_vars_idx <- function(data, ...){
  all_vars <- names(data)
  q <- quos(...)
  vars <- if( !length(q) ) {
    seq_along(all_vars)
  } else {
    match(select_vars(all_vars, !!!q), all_vars)
  }
}

count_na <- function(data, ...){
  par_count_na_cpp__impl( data, select_vars_idx(data, ...))
}

add_n_miss_label <- function(q, label){
  suffix <- if(length(q)) "_vars" else "_all"
  paste0(label, suffix )
}

#' Add column containing number of missing data values
#'
#' It can be useful when doing data analysis to add the number of missing data
#'   points into your dataframe. `add_n_miss` adds a column named "n_miss",
#'   which contains the number of missing values in that row.
#'
#' @param data a dataframe
#' @param ... Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label character default is "n_miss".
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_n_miss()
#' airquality %>% add_n_miss(Ozone, Solar.R)
#' airquality %>% add_n_miss(dplyr::contains("o"))
#'
#'
add_n_miss <- function(data, ..., label = "n_miss"){
  q <- quos(...)
  label <- add_n_miss_label(q,label)
  mutate( data, !!label := count_na( data, !!!q ) )
}

#' Add column containing proportion of missing data values
#'
#' It can be useful when doing data analysis to add the proportion of missing
#'   data values into your dataframe. `add_prop_miss` adds a column named
#'   "prop_miss", which contains the proportion of missing values in that row.
#'   You can specify the variables that you would like to show the missingness
#'   for.
#'
#' @param data a dataframe
#' @param ... Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label character string of what you need to name variable
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_prop_miss()
#'
#' airquality %>% add_prop_miss(Solar.R)
#'
#' airquality %>% add_prop_miss(Solar.R, Ozone)
#'
#' airquality %>% add_prop_miss(Solar.R, Ozone, label = "testing")
#'
#' # this can be applied to model the proportion of missing data
#' # as in Tierney et al bmjopen.bmj.com/content/5/6/e007450.full
#' library(rpart)
#' library(rpart.plot)
#'
#' airquality %>%
#' add_prop_miss() %>%
#' rpart(prop_miss_all ~ ., data = .) %>%
#' prp(type = 4,
#'     extra = 101,
#'     prefix = "prop_miss = ")

add_prop_miss <- function(data, ..., label = "prop_miss"){

  if (missing(...)) {
    purrrlyr::by_row(.d = data,
                     ..f = function(x) (mean(is.na(x))),
                     .collate = "row",
                     .to = paste0(label,"_all"))
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                           ..f = function(x) prop_miss(x),
                                           .collate = "row",
                                           .to =  paste0(label,"_vars"))

    # add only the variables prop_miss function, not the whole data.frame...
    prop_selected_data_cut <- prop_selected_data %>%
      dplyr::select(!!as.name(paste0(label,"_vars")))

    dplyr::bind_cols(data, prop_selected_data_cut) %>% dplyr::as_tibble()

  }
}
