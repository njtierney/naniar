# as_shadow errors when given non dataframe or 0 entry

    Code
      as_shadow(0)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      as_shadow("a")
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      as_shadow(matrix(airquality))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      as_shadow(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

