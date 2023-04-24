# shadow_shift returns an error when given the wrong kind of object

    Code
      shadow_shift(as.POSIXct(111, origin = "1970-01-01"))
    Error <rlang_error>
      Unsupported class provided to `shadow_shift()`:
      <class(x)>
      Check if your input is more than length one, and that you are using
      the right function.

