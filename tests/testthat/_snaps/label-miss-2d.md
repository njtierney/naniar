# label_miss_2d errors on the first NULL entry

    Code
      label_miss_2d(NULL, 3)
    Error <simpleError>
      Input cannot be NULL

# label_miss_2d errors on the second NULL entry

    Code
      label_miss_2d(3, NULL)
    Error <simpleError>
      Input cannot be NULL

# label_miss_2d errors when both are NULL

    Code
      label_miss_2d(NULL, NULL)
    Error <simpleError>
      Input cannot be NULL

# label_miss_2d identifies the correct location of missingness

    Code
      label_miss_2d(test_df$x, test_df$y)
    Output
      [1] Missing     Missing     Not Missing
      Levels: Missing Not Missing

---

    Code
      label_miss_2d(test_df$y, test_df$z)
    Output
      [1] Not Missing Missing     Not Missing
      Levels: Missing Not Missing

---

    Code
      label_miss_2d(test_df$x, test_df$z)
    Output
      [1] Missing     Not Missing Not Missing
      Levels: Missing Not Missing

