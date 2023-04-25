# nabular errors when given non dataframe or 0 entry

    Code
      nabular(0)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      nabular("a")
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      nabular(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      nabular(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

