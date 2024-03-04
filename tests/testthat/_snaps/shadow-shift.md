# shadow_shift returns soft deprecation warning

    Code
      shadow_shift(NULL)
    Condition
      Warning:
      `shadow_shift()` was deprecated in naniar 1.1.0.
      i Please use `impute_below()` instead.
    Output
      NULL

---

    Code
      shadow_shift(0+3i)
    Condition
      Warning:
      `shadow_shift()` was deprecated in naniar 1.1.0.
      i Please use `impute_below()` instead.
      Error in `impute_below()`:
      ! `impute_below()` does not know how to deal with data of class <complex>
      Check if your input is more than length one, and that you are using the right function. Perhaps you meant to apply this to many variables in a data frame? See the examples dor details on doing this with `across()`

---

    `shadow_shift()` was deprecated in naniar 1.1.0.
    i Please use `impute_below()` instead.

# shadow_shift still works

    Code
      shadow_shift(NULL)
    Output
      NULL

---

    Code
      shadow_shift(0+3i)
    Condition
      Error in `impute_below()`:
      ! `impute_below()` does not know how to deal with data of class <complex>
      Check if your input is more than length one, and that you are using the right function. Perhaps you meant to apply this to many variables in a data frame? See the examples dor details on doing this with `across()`

---

    Code
      shadow_shift(miss_vec_5)
    Output
      [1] 10.000000 10.000000  9.000000  2.163794  3.000000

