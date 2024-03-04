# shade errors with NULLs

    Code
      shade(NULL)
    Condition
      Error in `shade()`:
      ! Input must not be NULL
      Input is <NULL>

# shade errors with objects of length 0

    Code
      shade(numeric(0))
    Condition
      Error in `shade()`:
      ! input to shade must have length > 0

# shade errors with list of length 0

    Code
      shade(list())
    Condition
      Error in `shade()`:
      ! input to shade must have length > 0

# shade returns the correct values with list columns

    Code
      shade(list(3, list(1), c(2, 3), list()), broken = 3)
    Condition
      Error in `shade()`:
      ! additional levels of missing are not available when shade-ing lists column

