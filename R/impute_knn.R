#' Impute nearest neighbours
#'
#' @param .tbl data.frame with missing values
#' @param k integer, default value 5
#' @param method - "mean" or "random" - do you want to take the mean of the k
#'   neighbours, do you want to take a random value from the k neighbours.
#'
#' @return data.frame with imputed values
#'
#' @note need to add separate functions for knn_avg and knn_rand, and also
#'   scoped variants. Ideally, this should work on a vector first, with
#'   different methods for each part
#'
#' @export
#'
#' @examples
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_knn("mean")
#'
#'\dontrun{
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_knn() %>%
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'          geom_point()
#'}
impute_knn <- function(.tbl,
                       k = 5,
                       method = "mean"){

  shadow_exists <- any_shadow(.tbl)

  if (shadow_exists) {
    .shadow <- unbind_data(.tbl)
    .tbl <- unbind_shadow(.tbl)
  }

    test_if_dataframe(.tbl)
  # test if more than one variable for impute_knn_avg
  # test if there are only missing values
  # test if there are factors/characters - really?

# convert ordered variables to integer
.tbl <- dplyr::mutate_if(.tbl, is.ordered, as.integer)

complete_data <- .tbl[complete.cases(.tbl), ]
dat_knn_avg <- .tbl # aka dat$d1
dat_knn_rand <- .tbl #aka dat$d2

which_cases_complete <- which(!complete.cases(.tbl))

for (i in which_cases_complete) {

  which_var_complete <- which(!is.na(.tbl[i, ]))
  which_var_miss <- which(is.na(.tbl[i, ]))
  n_var_complete <- length(which_var_complete)

  # if there are columns with complete values
  if (n_var_complete > 0) {

    # use complete data as the neighbour dataset
    dat_nn <- complete_data

    # row bind the first row of data with missing values to complete data
    vec_distances <- rbind(.tbl[i, ], dat_nn) %>%
      # keep only columns with missings, keep as data.frame (drop = FALSE)
      .[ , which_var_complete, drop = FALSE] %>%
      # scale the variables
      apply(2, scale) %>%
      # get the distances
      dist() %>%
      # get the actual distance values
      .[1:nrow(dat_nn)]

    # get distances of the complete data
    dat_nn$distance <- vec_distances

    # reorder complete data according to the distance
    # take "k" values (or nrow(complete) if this is smaller than "k")
    k_crit <- min(k, nrow(dat_nn))

    dat_k_nn <- dat_nn %>%
      dplyr::arrange(distance) %>%
      dplyr::slice(1:k_crit)

    # imputation part
    # Using the dat_k_nn, take the mean of the columns, for columns with missing
    # values.

    knn_avg <- dat_k_nn[ , 1:ncol(.tbl)] %>%
        .[ , which_var_miss, drop = FALSE] %>%
        colMeans()

    dat_knn_avg[i, which_var_miss] <- knn_avg

    # the random neighbough imputation part

    k_rand_row <- sample(1:nrow(dat_k_nn), 1)

    knn_rand <- dat_k_nn[ , 1:ncol(.tbl)][k_rand_row, which_var_miss]

    dat_knn_rand[i, which_var_miss] <- knn_rand

    # else, if there aren't complete columns - just do mean imputation
    # wait, really?
  } else if (n_var_complete == 0) {
    dat_knn_avg[i,] = sapply(.tbl, mean, na.rm = TRUE)
    dat_knn_rand[i,] = sapply(.tbl, function(x) sample(na.omit(x), 1))
  }
}

# round things for the ordered factors...

ordered_factor <- which(unname(purrr::map_chr(.tbl, class)) == "ordered")

if (length(ordered_factor)) {
  for (j in ordered_factor) {
    dat_knn_avg[ ,j] = round(dat_knn_avg[ ,j])
    l = levels(.tbl[ ,j])
    dat_knn_avg[ ,j] = ordered(l[dat_knn_avg[ ,j]], levels = l)
    dat_knn_rand[ ,j] = ordered(l[dat_knn_rand[ ,j]], levels = l)
  }
}

# reattach the shadow
if (shadow_exists) {
  dat_knn_avg <- dplyr::bind_cols(dat_knn_avg, .shadow)
  dat_knn_rand <- dplyr::bind_cols(dat_knn_rand, .shadow)
}

if (method == "random") return(dat_knn_rand)
if (method == "mean") return(dat_knn_avg)

}
