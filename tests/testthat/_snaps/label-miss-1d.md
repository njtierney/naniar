# label_miss_1d errors on a NULL entry

    Code
      label_miss_1d(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# label_miss_1d identifies the correct location of missingness

    Code
      label_miss_1d(test_df$x)
    Output
      [1] Missing     Not Missing Not Missing
      Levels: Missing Not Missing

---

    Code
      label_miss_1d(test_df$y)
    Output
      [1] Not Missing Missing     Not Missing
      Levels: Missing Not Missing

---

    Code
      label_miss_1d(test_df$z)
    Output
      [1] Not Missing Not Missing Not Missing
      Levels: Missing Not Missing

