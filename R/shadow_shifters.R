#' shadow_cat
#'
#' \code{shadow_cat} reorganizes
#'
#' @description shadow_cat is a utility function that rearranges factor levels
#'
#' @param x a dataframe coming from `interaction`
#'
#' @export
# let's then make this a window function...shadow cat

shadow_cat <- function(x){
  ifelse(x == "TRUE.FALSE" |
           x == "TRUE.TRUE" |
           x == "FALSE.TRUE",
         yes = "Missing",
         no = "Not Missing")
}

#' shadow_shift
#'
#' shadow_shift is a window function that transforms missing values to be about 10% below the minimum value for a given variable, plus some jittered noise, to separate repeated values, so that missing values can be visualised along with the rest of the data
#'
#' @param x is a variable, must be continuous
#'
# ======================
# Constructor function
# ======================

# create the S3 method
#' @export
shadow_shift <- function(x) UseMethod("shadow_shift")

# =====
# NULL
# =====

#' @export
shadow_shift.NULL <- function(x) NULL

# =====
# default
# =====

#' @export
shadow_shift.default <- function(x){
  stop(
    "shadow_shift does not know how to deal with data of class ",
    class(x)
  )

}

#' @export
shadow_shift.numeric <- function(x){

  xrange <- max(x, na.rm = T) - min(x, na.rm = T)

  xmin <- min(x, na.rm = T)

  # create the "jitter" to be added around the points.
  xrunif <- (runif(length(x))-0.5)*xrange*0.05

  ifelse(is.na(x),
         # add the jitter around the those values that are missing
         yes = xmin-xrange*0.1 + xrunif,
         no = x)

} # close function

#' shadow_df
#'
#' @description \code{shadow_df} creates a shadow matrix/data frame of class 'tbl_df' that denotes whether a given cell is missing or not - if a value is missing, it is denoted as TRUE
#'
#' @param x a dataframe
#'
#' @import dplyr
#'
#' @export

shadow_df <- function (x){
  y <- if (length(x)) {
    dplyr::as_data_frame(
      lapply(x, "is.na")
    )
  }
  else dplyr::data_frame()
  y
}


#' miss_cat
#'
#' \code{miss_cat} creates factor levels of missingness for two variables
#'
#' @description miss_cat takes a data frame `df`, and two variables as strings, `var1` and `var2`, and converts them to a missing TRUE/FALSE matrix, where TRUE = missing. It then uses `interaction`, to create all the different levels of missingness - TRUE.TRUE, TRUE.FALSE, FALSE.TRUE, and FALSE.FALSE. It then uses the function `shadow_cat` to collapse across these and put them into the relevant categories
#'
#' @param df a dataframe
#'
#' @param var1 a dataframe
#'
#' @param var2 a dataframe
#'
#' @import dplyr
#'
#' @export
miss_cat <- function(df, var1, var2){
  df %>%
    # make the data into a true/false data frame
    shadow_df %>%
    # choose the variables of interest
    dplyr::select(dplyr::one_of(var1, var2)) %>%
    # get all the combinations of the levels as factors
    interaction %>%
    # combine them into something sensible for our purposes
    shadow_cat
}
