# miss_var_table errors on NULL

    Code
      miss_var_table(NULL)
    Condition
      Error in `miss_var_table()`:
      ! Input must not be NULL
      Input is <NULL>

# miss_var_table errors when a non-dataframe given

    Code
      miss_var_table(1)
    Condition
      Error in `miss_var_table()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_var_table("a")
    Condition
      Error in `miss_var_table()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_var_table(matrix(0))
    Condition
      Error in `miss_var_table()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

