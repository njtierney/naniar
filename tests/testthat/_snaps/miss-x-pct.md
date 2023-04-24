# prop_miss* errors on NULL

    Code
      prop_miss(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      prop_miss_var(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      prop_miss_case(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# pct_miss* errors on NULL

    Code
      pct_miss(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      pct_miss_var(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      pct_miss_case(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# prop_complete* errors on NULL

    Code
      prop_complete(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      prop_complete_var(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      prop_complete_case(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# pct_complete* errors on NULL

    Code
      pct_complete(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      pct_complete_var(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

---

    Code
      pct_complete_case(NULL)
    Error <rlang_error>
      Input must not be NULL
      Input is <NULL>

# prop_miss* errors when a non-dataframe given

    Code
      prop_miss_var(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      prop_miss_var(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      prop_miss_case(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      prop_miss_case(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

# prop_complete* errors when a non-dataframe given

    Code
      prop_complete_var(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      prop_complete_var(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      prop_complete_case(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      prop_complete_case(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

# pct_miss* errors when a non-dataframe given

    Code
      pct_miss_var(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      pct_miss_var(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      pct_miss_case(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      pct_miss_case(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

# pct_complete* errors when a non-dataframe given

    Code
      pct_complete_var(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      pct_complete_var(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

---

    Code
      pct_complete_case(1)
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <numeric>

---

    Code
      pct_complete_case(matrix(0))
    Error <rlang_error>
      Input must inherit from <data.frame>
      We see class: <matrix/array>

