# impute_below_all errors when given wrong object

    Code
      impute_below_all(as.POSIXct(111, origin = "1970-01-01"))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <POSIXct/POSIXt>

