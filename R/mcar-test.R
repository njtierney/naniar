#' Little's missing completely at random (MCAR) test
#'
#' Use Little's (1988) test statistic to assess if data is missing completely
#'   at random (MCAR). The null hypothesis in this test is that the data is
#'    MCAR, and the test statistic is a chi-squared value. The example below
#'    shows the output of `mcar_test(airquality)`. Given the high statistic
#'    value and low p-value, we can conclude the `airquality` data is not
#'    missing completely at random.
#'
#' @param data A data frame
#'
#' @return A [tibble::tibble()] with one row and four columns:
#' \item{statistic}{Chi-squared statistic for Little's test}
#' \item{df}{Degrees of freedom used for chi-squared statistic}
#' \item{p.value}{P-value for the chi-squared statistic}
#' \item{missing.patterns}{Number of missing data patterns in the data}
#'
#' @references Little, Roderick J. A. 1988. "A Test of Missing Completely at
#'   Random for Multivariate Data with Missing Values."
#'   *Journal of the American Statistical Association* 83 (404):
#'   1198--1202. <https://doi.org/10.1080/01621459.1988.10478722>.
#'
#' @note Code is adapted from LittleMCAR() in the now-orphaned {BaylorEdPsych}
#'   package: https://rdrr.io/cran/BaylorEdPsych/man/LittleMCAR.html. Some of
#'   code is adapted from Eric Stemmler - <https://stats-bayes.com/post/2020/08/14/r-function-for-little-s-test-for-data-missing-completely-at-random/>
#'   using Maximum likelihood estimation from {norm}.
#'
#' @author Andrew Heiss, \email{andrew@andrewheiss.com}
#'
#' @examples
#' mcar_test(airquality)
#' mcar_test(oceanbuoys)
#'
#' # If there are non-numeric columns, there will be a warning
#' mcar_test(riskfactors)
#'
#' @importFrom rlang .data
#'
#' @export
mcar_test <- function(data) {

  test_if_dataframe(data)

  # norm::prelim.norm needs to work with a data.matrix
  data <- data.matrix(data)

  # Number of variables in data
  n_var <- ncol(data)

  # Number of rows
  n <- nrow(data)
  var_names <- colnames(data)

  # Calculate pattern of missingness for each row
  r <- 1 * is.na(data)
  mdp <- (r %*% (2^((1:n_var - 1)))) + 1

  # Add pattern as column to original data
  x_miss_pattern <- data.frame(cbind(data, mdp))
  colnames(x_miss_pattern) <- c(var_names, "miss_pattern")

  # Number of unique missing data patterns
  n_miss_pattern <- length(unique(x_miss_pattern$miss_pattern))

  # Recode miss_pattern variable to go from 1 through n_miss_pattern
  x_miss_pattern <- x_miss_pattern %>%
    dplyr::mutate(miss_pattern = dplyr::dense_rank(.data$miss_pattern))

  # Maximum likelihood estimation from {norm}
  # Adapted from Eric Stemmler
  # https://stats-bayes.com/post/2020/08/14/r-function-for-little-s-test-for-data-missing-completely-at-random/
  s <- norm::prelim.norm(data)
  ll <- norm::em.norm(s, showits = FALSE)
  fit <- norm::getparam.norm(s = s, theta = ll)
  grand_mean <- fit$mu
  grand_cov <- fit$sigma
  colnames(grand_cov) <- rownames(grand_cov) <- colnames(data)

  little_calculations <- x_miss_pattern %>%
    # For each of the types of missing patterns...
    dplyr::group_by(.data$miss_pattern) %>%
    tidyr::nest() %>%
    # kj terms for degrees of freedom
    dplyr::mutate(
      kj = purrr::map_dbl(.data$data,
                          ~colSums(as.matrix(1 * !is.na(colSums(.x)))))
    ) %>%
    # Calculate column averages
    dplyr::mutate(
      mu = purrr::map(.data$data, ~colMeans(.x) - grand_mean),
      mu = purrr::map(.data$mu, ~.x[!is.na(.x)])
    ) %>%
    # Binary 0/1 indicating if column should be kept
    dplyr::mutate(
      keep = purrr::map(.data$data, ~1 * !is.na(colSums(.x))),
      keep = purrr::map(.data$keep, ~.x[which(.x[1:n_var] != 0)])
    ) %>%
    # Drop rows and columns from global covariance matrix so that the matrix
    # only contains rows and columns that exist in current missing pattern
    dplyr::mutate(
      sigma = purrr::map(.data$keep,
                         ~grand_cov[which(rownames(grand_cov) %in% names(.x)),
                                    which(colnames(grand_cov) %in% names(.x))])
    ) %>%
    # Finally calculate Little's statistic using the number of rows in missing
    # pattern, average, and covariance
    dplyr::mutate(
      d2 = purrr::pmap_dbl(
        list(.data$data, .data$mu, .data$sigma, .data$kj),
        ~ifelse(..4 == 0,
                0,  # If the pattern is all NA, use 0
                nrow(..1) * (t(..2) %*% solve(..3) %*% ..2))
      )) %>%
    dplyr::ungroup()

  # Main calculations
  d2 <- sum(little_calculations$d2)  # Little's d2
  df <- sum(little_calculations$kj) - n_var  # Degrees of freedom
  p_value <- 1 - stats::pchisq(d2, df)  # p-value

  # Return everything as a glance-like tibble
  tibble::tibble(statistic = d2, df = df, p.value = p_value,
                 missing.patterns = n_miss_pattern)
}
