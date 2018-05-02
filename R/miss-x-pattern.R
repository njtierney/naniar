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

  test_if_dataframe(data)

  test_if_null(data)


dat_pat <- data %>%
  mice::md.pattern() %>%
  as.data.frame() %>%
  tibble::as_tibble(.) %>%
  tibble::rownames_to_column(., var = "n_cases") %>%
  dplyr::mutate_if(is.character, as.integer) %>%
  dplyr::select(names(data),n_cases, V7)

# reset the last name (there has got to be a more elegant way of doing this?)
colnames(dat_pat)[length(names(dat_pat))] <- "n_miss"

dat_pat %>%
  dplyr::select(names(dat_pat), n_miss, n_cases) %>%
  # drop the last row (!)
  head( -1)
}

# need to reimplement the mice::md.pattern code
#
# if (!(is.matrix(x) || is.data.frame(x)))
#   stop("Data should be a matrix or dataframe")
# if (ncol(x) < 2)
#   stop("Data should have at least two columns")
# if (is.data.frame(x))
#   x <- data.matrix(x)
# n <- nrow(x)
# p <- ncol(x)
# mode(x) <- "single"
# r <- 1 * is.na(x)
# nmis <- as.integer(apply(r, 2, sum))
# names(nmis) <- dimnames(x)[[2]]
# mdp <- (r %*% (2^((seq_len(ncol(x))) - 1))) + 1
# ro <- order(mdp)
# x <- matrix(x[ro, ], n, p)
# mdp <- mdp[ro]
# r <- matrix(r[ro, ], n, p)
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
# r <- rbind(r[ro2, co], nmis[co])
# r <- cbind(r, c(nmis.row[ro2], sum(nmis)))
# r
