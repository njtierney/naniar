# impute_median and scoped variants fail when given wrong input

    Code
      impute_median_if(numeric(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      impute_median_at(numeric(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      impute_median_all(numeric(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      impute_median_if(NULL)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <NULL>

---

    Code
      impute_median_at(NULL)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <NULL>

---

    Code
      impute_mean_all(NULL)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <NULL>

