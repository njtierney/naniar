# impute_below returns an error when given the wrong kind of object

    Code
      impute_below(as.POSIXct(111, origin = "1970-01-01"))
    Error <rlang_error>
      `impute_below()` does not know how to deal with data of class <POSIXct, or POSIXt>
      Check if your input is more than length one, and that you are using the right function. Perhaps you meant to apply this to many variables in a data frame? See the examples dor details on doing this with `across()`

# impute_below_all errors when given wrong object

    Code
      impute_below_all(as.POSIXct(111, origin = "1970-01-01"))
    Error <simpleError>
      Input must inherit from data.frame

