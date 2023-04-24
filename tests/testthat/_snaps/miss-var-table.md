# miss_var_table errors on NULL

    Code
      miss_var_table(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# miss_var_table errors when a non-dataframe given

    Code
      miss_var_table(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_var_table("a")
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_var_table(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

