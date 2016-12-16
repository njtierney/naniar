
# give NAs a more meaningful label
is_na <- function(x) {
  factor(is.na(x), levels = c(FALSE, TRUE), labels = c("!NA", "NA"))
}

# append some shadow cols

# return a tibble that is a shadow matrix form.

as_shadow <- function(data){

  data_shadow <- purrr::map_df(data, is_na)

  names(data_shadow) <- paste0(names(data),"_NA")

  data_shadow

}

# as_shadow(airquality)

bind_shadow <- function(data){

  # data_shadow <- map_df(data, is_na)
  # names(data_shadow) <- paste0(names(data),"_NA")
  data_shadow <- as_shadow(data)

  bound_shadow <- tibble::as_tibble(dplyr::bind_cols(data, data_shadow))

  bound_shadow

}

ggplot(data = bind_shadow(airquality),
       aes(x = Ozone)) +
  geom_histogram() +
  facet_wrap(~Solar.R_NA,
             ncol = 1)
#
ggplot(data = airquality,
       aes(x = Ozone)) +
  geom_histogram() +
  facet_wrap(~is_na(Solar.R),
             ncol = 1)
#
# ggplot(bind_shadow(airquality),
#        aes(x = Ozone,
#            colour = Solar.R_NA)) +
#   geom_density() +
#   geom_rug()
#
# ggplot(airquality,
#        aes(x = Ozone,
#            colour = is_na(Solar.R))) +
#   geom_density() +
#   geom_rug()

# other "long" shadow format

# shadow_join exists as we want to include this extra metadata about the rows that have missing data, but I also wanted to include some extra information about the class of the data, in case we need to gather the data back into a wider, rather than long, format. Here it takes a function `visdat`, `visdat:::fingerprint`, which is currently not a particularly complex function.

gather_shadow <- function(df){

  df_val_type <- df %>%
    as_data_frame() %>%
    purrr::dmap(visdat:::fingerprint) %>%
    mutate(rows = row_number()) %>%
    tidyr::gather_(key_col = "variable",
                   value_col = "valueType",
                   gather_cols = names(.)[-length(.)])

  # df_shadow
  df_shadow <- df %>%
    as_data_frame() %>%
    mutate(rows = row_number()) %>%
    gather(key = variable,
           value = value,
           -rows) %>%
    mutate(shadow_matrix = is_na(value)) %>%
    left_join(df_val_type)

  # perhaps define some attributes

}

aq_shadow <- gather_shadow(airquality)
