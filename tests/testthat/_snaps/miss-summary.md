# miss_summary errors when given non dataframe or 0 entry

    Code
      miss_summary(0)
    Condition
      Error in `miss_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_summary("a")
    Condition
      Error in `miss_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_summary(matrix(airquality))
    Condition
      Error in `miss_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      miss_summary(NULL)
    Condition
      Error in `miss_summary()`:
      ! Input must not be NULL
      Input is <NULL>

