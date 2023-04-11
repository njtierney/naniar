# shadow_shift returns soft deprecation warning

    Code
      shadow_shift(NULL)
    Warning <lifecycle_warning_deprecated>
      `shadow_shift()` was deprecated in naniar 1.1.0.
      i Please use `impute_below()` instead.
    Output
      NULL

---

    Code
      shadow_shift(as.POSIXct(111, origin = "1970-01-01"))
    Warning <lifecycle_warning_deprecated>
      `shadow_shift()` was deprecated in naniar 1.1.0.
      i Please use `impute_below()` instead.
    Error <rlang_error>
      `impute_below()` does not know how to deal with data of class <POSIXct, or POSIXt>
      Check if your input is more than length one, and that you are using the right function. Perhaps you meant to apply this to many variables in a data frame? See the examples dor details on doing this with `across()`

---

    Code
      shadow_shift(miss_vec_5)
    Warning <lifecycle_warning_deprecated>
      `shadow_shift()` was deprecated in naniar 1.1.0.
      i Please use `impute_below()` instead.
    Output
      [1] 10.000000 10.000000  9.000000  2.163794  3.000000

