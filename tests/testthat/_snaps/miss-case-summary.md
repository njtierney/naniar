# miss_case_summary errors on NULL

    Code
      miss_case_summary(NULL)
    Condition
      Error in `miss_case_summary()`:
      ! Input must not be NULL
      Input is <NULL>

# miss_case_summary errors when a non-dataframe given

    Code
      miss_case_summary(1)
    Condition
      Error in `miss_case_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_case_summary("a")
    Condition
      Error in `miss_case_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_case_summary(matrix(0))
    Condition
      Error in `miss_case_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

