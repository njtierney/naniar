# miss_prop_summary errors when given non dataframe or 0 entry

    Code
      miss_prop_summary(0)
    Condition
      Error in `miss_prop_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      miss_prop_summary("a")
    Condition
      Error in `miss_prop_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <character>

---

    Code
      miss_prop_summary(matrix(airquality))
    Condition
      Error in `miss_prop_summary()`:
      ! Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      miss_prop_summary(NULL)
    Condition
      Error in `miss_prop_summary()`:
      ! Input must not be NULL
      Input is <NULL>

