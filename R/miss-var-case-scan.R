# library(naniar)
# library(tidyverse)
#
# dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#                           3,   "N/A",  -99,
#                           -99, "E",    97,
#                           -98, "F",    97)
#
# n_match <- function(x, search){
#   sum(grepl(search, x))
# }
#
# n_match(dat_ms, -99)
#
# dat_ms %>%
#   map(n_match,-99)
#
# dat_ms %>%
#   map(n_match,-98)
#
# dat_ms %>%
#   map(n_match,"N/A")
#
# # ideal
# dat_ms %>%
#   map(n_match,
#       list("N/A",
#            -99))
#
# v1 <- dat_ms %>%
#   map(n_match,-99) %>%
#   t() %>% t()
#
# v2 <- dat_ms %>%
#   map(n_match,-98) %>%
#   t() %>% t()
#
# v3 <- dat_ms %>%
#   map(n_match,"N/A") %>%
#   t() %>% t()
#
# my_mat <- cbind(v1, v2, v3)
#
# colnames(my_mat) <- c("-99", "-98", "N/A")
#
# rownames(my_mat)
#
# as_tibble(my_mat) %>%
#   unnest() %>%
#   rownames_to_column()
#
# dat_ms %>%
#   map(n_match, list(-99,
#                    -98))
#
# n_match(list(-99,-98), dat_ms)
#
# dat_ms %>%
#   summarise_all(~sum(grepl(search,.x), na.rm = TRUE))
#
# dat_ms %>%
#   purrr::map_df(~length(grep(paste0("N/A", collapse ="|"),x = .))) %>%
#   tidyr::gather(key = "Variable",
#                 value = "n")
