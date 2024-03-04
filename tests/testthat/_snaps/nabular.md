# nabular errors when given non dataframe or 0 entry

    Code
      nabular(0)
    Condition
      Error in `as_shadow()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      nabular("a")
    Condition
      Error in `as_shadow()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      nabular(matrix(0))
    Condition
      Error in `as_shadow()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      nabular(NULL)
    Condition
      Error in `as_shadow()`:
      ! Input must not be NULL
      Input is <NULL>

