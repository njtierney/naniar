# impute_median and scoped variants fail when given wrong input

    Code
      impute_median_if(numeric(0))
    Condition
      Error in `impute_median_if()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      impute_median_at(numeric(0))
    Condition
      Error in `impute_median_at()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      impute_median_all(numeric(0))
    Condition
      Error in `impute_median_all()`:
      ! Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      impute_median_if(NULL)
    Condition
      Error in `impute_median_if()`:
      ! Input must inherit from <data.frame>
      We see class: <NULL>

---

    Code
      impute_median_at(NULL)
    Condition
      Error in `impute_median_at()`:
      ! Input must inherit from <data.frame>
      We see class: <NULL>

---

    Code
      impute_mean_all(NULL)
    Condition
      Error in `impute_mean_all()`:
      ! Input must inherit from <data.frame>
      We see class: <NULL>

