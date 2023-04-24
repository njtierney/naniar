# gather_shadow errors when given non dataframe or 0 entry

    Code
      gather_shadow(0)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      gather_shadow(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      gather_shadow(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

