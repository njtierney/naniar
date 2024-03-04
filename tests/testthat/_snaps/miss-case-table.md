# miss_case_table errors when given wrong type

    Code
      miss_case_table(NULL)
    Condition
      Error in `miss_case_table()`:
      ! Input must not be NULL
      Input is <NULL>

---

    Code
      miss_case_table(1)
    Condition
      Error in `miss_case_table()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_case_table("a")
    Condition
      Error in `miss_case_table()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_case_table(matrix(0))
    Condition
      Error in `miss_case_table()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

