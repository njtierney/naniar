# as_shadow_upset errors when given datasets with <= 1 variables

    Code
      as_shadow_upset(diag_na(1))
    Error <rlang_error>
      upset plots for missing data requre at least two variables to have missing data, only one variable, 'x1' has missing values.

---

    Code
      as_shadow_upset(data.frame(x = NA))
    Error <rlang_error>
      upset plots for missing data requre at least two variables to have missing data, only one variable, 'x' has missing values.

---

    Code
      as_shadow_upset(data.frame(numeric(0)))
    Error <rlang_error>
      upset plots for missing data requre at least two variables to have missing data, there are no missing values in your data! This is probably a good thing.

# as_shadow_upset errors when given non dataframe or 0 entry

    Code
      as_shadow_upset(0)
    Error <simpleError>
      'x' must be an array of at least two dimensions

---

    Code
      as_shadow_upset("a")
    Error <simpleError>
      'x' must be an array of at least two dimensions

---

    Code
      as_shadow_upset(matrix(airquality))
    Error <rlang_error>
      upset plots for missing data requre at least two variables to have missing data, there are no missing values in your data! This is probably a good thing.

---

    Code
      as_shadow_upset(NULL)
    Error <simpleError>
      'x' must be an array of at least two dimensions

