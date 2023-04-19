# miss_case_summary errors on NULL

    Code
      miss_case_summary(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# miss_case_summary errors when a non-dataframe given

    Code
      miss_case_summary(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_case_summary("a")
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_case_summary(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

