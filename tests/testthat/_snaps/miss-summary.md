# miss_summary errors when given non dataframe or 0 entry

    Code
      miss_summary(0)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_summary("a")
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_summary(matrix(airquality))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      miss_summary(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

