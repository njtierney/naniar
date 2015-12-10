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
