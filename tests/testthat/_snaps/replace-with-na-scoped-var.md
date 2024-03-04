# empty call provides an error

    Code
      replace_with_na_all(df)
    Condition
      Error in `map()`:
      i In index: 1.
      i With name: x.
      Caused by error:
      ! Failed to evaluate glue component {rlang::f_text(condition)}
      Caused by error in `.f()`:
      ! argument "condition" is missing, with no default

