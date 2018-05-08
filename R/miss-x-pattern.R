#' Calculate the pattern of missingness
#'
#' @param data data.frame
#'
#' @return data.frame of missingness patterns
#' @export
#'
#' @examples
#'
#' miss_pattern(airquality)
miss_pattern <- function(data){

  # defensive coding --- --- --- --- --- --- ---
  test_if_dataframe(data)
  test_if_null(data)
  test_if_two_cols(data)

  # convert data.frame into numeric format.


dat_pat <- data %>%
  mice::md.pattern() %>%
  as.data.frame() %>%
  tibble::as_tibble(.) %>%
  tibble::rownames_to_column(., var = "n_cases") %>%
  dplyr::mutate_if(is.character, as.integer)
  # dplyr::select(names(data),n_cases, dplyr::starts_with("V"))

# reset the last name (there has got to be a more elegant way of doing this?)
colnames(dat_pat)[length(names(dat_pat))] <- "n_miss"

dat_pat %>%
  # dplyr::select(names(dat_pat), n_miss, n_cases) %>%
  # drop the last row (!)
  head( -1)
}

# # need to reimplement the mice::md.pattern code
# #
# # convert data.frame to a numeric matrix ---------------------------------------
# # x = airquality
# x = data.frame(x = c(NA, 1, 2, 3),
#                y = c(1, NA, 2, 3),
#                z = c(NA, NA, 1, NA))
# # #
# x <- data.matrix(x)
# n_rows <- nrow(x)
# p_cols <- ncol(x)
#
# # ---- mode? what?
# # mode(x) <- "single"
#
# # create the numeric missingness matrix
# r_mat <- 1 * is.na(x)
#
# # get the number of missings for each variable ---------------------------------
#
# # nmis
# n_miss_cols <- colSums(r)
# names(n_miss_cols) <- dimnames(x)[[2]]
# #
# # # this section reorders the initial matrix such that the patterns are laid out
# # # so for airquality, the first 1111 rows all have no missing values, and so on.
# #
# col_sequence <- seq_len(p_cols) - 1
# #
# bit_id <- 2 ^ col_sequence
#
# mdp <- (r_mat %*% bit_id) + 1
#
# ro <- order(mdp)
#
# x <- matrix(data = x[ro, ],
#             nrow = n_rows,
#             ncol = p_cols)
#
# # This then creates a vector containing, I think, the pattern of missings in each row sorted
# mdp <- mdp[ro]
#
#
# r <- matrix(r_mat[ro, ], n_rows, p_cols)
#
# ro <- order(ro)
#
# mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
#
# mdp <- unique(mdp)
#
# n_patterns <- length(mdpst)
#
# r <- 1 - r
#
# r <- matrix(r[mdpst, ], n_patterns, p_cols)
#
# if (npatt == 1)
#   tmp <- format(n_cols)
# if (npatt > 1)
#   tmp <- format(c(mdpst[2:n_patterns], n_rows + 1) - mdpst)
# dimnames(r) <- list(tmp, dimnames(x)[[2]])
#
# # as.integer is the same?
# storage.mode(r) <- "integer"
#
# if (npatt > 1)
#
#   nmdp <- as.integer(c(mdpst[-1], n_rows + 1) - mdpst)
#
# if (npatt == 1){
#   nmdp <- n
#   }
#
# co <- order(n_miss_cols)
# ro2 <- order(nmis.row <- p_cols - as.integer(apply(r, 1, sum)))
#
# # final formatting changes -----------------------------------------------------
# # r <- rbind(r[ro2, co], nmis[co])
# # r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
# # r
# #
# # miss_dat <- data.frame(x = c(NA, 1, 1),
# #                        y = c(1, NA, 1),
# #                        z = c(1, 1, NA))
# #
# # mice::md.pattern(miss_dat)
# # miss_pattern(miss_dat)
# # #
# # # miss_dat <- data.frame(x = rep(NA, 3),
# # #                        y = rep(NA, 3),
# # #                        z = rep(NA, 3))
# # #
# # # mice::md.pattern(miss_dat)
# #
# # miss_dat <- data.frame(x = c(NA, 1, 1),
# #                        y = c(1, NA, 1),
# #                        z = c(1, 1, NA))
# #
# # miss_pattern(airquality)
# #
# # miss_case_table(airquality)
# # miss_var_table(airquality)
# #
# # # unique_pattern
# # is.na(airquality)*1
#
# # ?unique
#
#
# # ---- missing pattern pairs
#
# # things to change
# # return a non-list structure as output
# # output should describe what rr, rm, mr, and mm actually are
# # consider breaking this into separate functions
# #
# # so the question then is, is there some way to get each of these to perform
# # separate functions that can be composed in a useful way?
# # Is there a visualisation that makes this make more sense?
# #
#
# miss_pattern_pairs <- function(data){
#
#   test_if_dataframe(data)
#
#   test_if_two_cols(data)
#
#   r_mat <- !is.na(data)
#
#   rr <- t(r_mat) %*% r_mat
#
#   mm <- t(!r_mat) %*% (!r_mat)
#
#   mr <- t(!r_mat) %*% r_mat
#
#   rm <- t(r_mat) %*% (!r_mat)
#
#   return(list(rr = rr, rm = rm, mr = mr, mm = mm))
#
# }
#
# miss_pattern_pairs(airquality)
