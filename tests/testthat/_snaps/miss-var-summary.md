# miss_var_summary errors when a non-dataframe given

    Code
      miss_var_summary(NULL)
    Condition
      Error in `miss_var_summary()`:
      ! Input must not be NULL
      Input is <NULL>

---

    Code
      miss_var_summary(matrix(0))
    Condition
      Error in `miss_var_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

