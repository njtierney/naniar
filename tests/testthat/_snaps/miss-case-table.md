# miss_case_table errors when given wrong type

    Code
      miss_case_table(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      miss_case_table(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_case_table("a")
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_case_table(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

