# recode_shadow errors when regular dataframe passed

    Code
      recode_shadow(df, temp = .where(wind == -99 ~ "bananas"))
    Error <dplyr:::mutate_error>
      i In argument: `temp_NA = structure(...)`.
      Caused by error in `dplyr::case_when()`:
      ! Failed to evaluate the right-hand side of formula 2.
      Caused by error:
      ! object 'temp_NA' not found

