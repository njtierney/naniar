# recode_shadow errors when regular dataframe passed

    Code
      recode_shadow(df, temp = .where(wind == -99 ~ "bananas"))
    Condition
      Error in `recode_shadow()`:
      ! Input must contain a shade column.
      See `?shade`, `?shade`, and `?bind_shadow`

