# as_shadow errors when given non dataframe or 0 entry

    Code
      as_shadow(0)
    Condition
      Error in `as_shadow()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      as_shadow("a")
    Condition
      Error in `as_shadow()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      as_shadow(matrix(airquality))
    Condition
      Error in `as_shadow()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      as_shadow(NULL)
    Condition
      Error in `as_shadow()`:
      ! Input must not be NULL
      Input is <NULL>

