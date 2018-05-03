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

# need to reimplement the mice::md.pattern code
#
# convert data.frame to a numeric matrix ---------------------------------------
# x = airquality
# #
# x <- data.matrix(x)
# n_rows <- nrow(x)
# p_cols <- ncol(x)

# ---- mode? what?
# mode(x) <- "single"

# create the numeric missingness matrix
# r <- 1 * is.na(x)

# get the number of missings for each variable ---------------------------------

# nmis
# n_miss_cols <- colSums(r)
# names(n_miss_cols) <- dimnames(x)[[2]]
#
# # this section reorders the initial matrix such that the patterns are laid out
# # so for airquality, the first 1111 rows all have no missing values, and so on.
#
# data = data.frame(x = c(NA, 1, 2, 3),
#                   y = c(1, 2, 3, NA),
#                   z = c(NA, NA, NA, NA))
#
# x = data.matrix(data)
#
# col_sequence <- seq_len(ncol(data)) - 1
#
# bit_id <- 2 ^ col_sequence
#
# r_mat <- is.na(x) * 1
#
# n_rows <- nrow(x)
# p_cols <- ncol(x)
#
# mdp <- (r_mat %*% (2^(seq_len(p_cols) - 1))) + 1
# ro <- order(mdp)
# x <- matrix(data = x[ro, ],
#             nrow = n_rows,
#             ncol = p_cols)
#
# # This then creates a vector containing, I think, the pattern of missings in each row sorted
# mdp <- mdp[ro]
#
# r_mat
# ro
#
# r <- matrix(r_mat[ro, ], n_rows, p_cols)
# ro <- order(ro)
# mdpst <- as.integer(seq(along = mdp)[!duplicated(mdp)])
# mdp <- unique(mdp)
# npatt <- length(mdpst)
# r <- 1 - r
# r <- matrix(r[mdpst, ], npatt, p)
# if (npatt == 1)
#   tmp <- format(n)
# if (npatt > 1)
#   tmp <- format(c(mdpst[2:npatt], n + 1) - mdpst)
# dimnames(r) <- list(tmp, dimnames(x)[[2]])
# storage.mode(r) <- "integer"
# if (npatt > 1)
#   nmdp <- as.integer(c(mdpst[-1], n + 1) - mdpst)
# if (npatt == 1)
#   nmdp <- n
# co <- order(nmis)
# ro2 <- order(nmis.row <- p - as.integer(apply(r, 1, sum)))

# final formatting changes -----------------------------------------------------
# r <- rbind(r[ro2, co], nmis[co])
# r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
# r
#
# miss_dat <- data.frame(x = c(NA, 1, 1),
#                        y = c(1, NA, 1),
#                        z = c(1, 1, NA))
#
# mice::md.pattern(miss_dat)
# miss_pattern(miss_dat)
# #
# # miss_dat <- data.frame(x = rep(NA, 3),
# #                        y = rep(NA, 3),
# #                        z = rep(NA, 3))
# #
# # mice::md.pattern(miss_dat)
#
# miss_dat <- data.frame(x = c(NA, 1, 1),
#                        y = c(1, NA, 1),
#                        z = c(1, 1, NA))
#
# miss_pattern(airquality)
#
# miss_case_table(airquality)
# miss_var_table(airquality)
#
# # unique_pattern
# is.na(airquality)*1

# ?unique
