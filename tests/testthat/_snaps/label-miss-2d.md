# label_miss_2d errors on the first NULL entry

    Code
      label_miss_2d(NULL, 3)
    Error <simpleError>
      Expecting '}'

# label_miss_2d errors on the second NULL entry

    Code
      label_miss_2d(3, NULL)
    Error <simpleError>
      Expecting '}'

# label_miss_2d errors when both are NULL

    Code
      label_miss_2d(NULL, NULL)
    Error <simpleError>
      Expecting '}'

