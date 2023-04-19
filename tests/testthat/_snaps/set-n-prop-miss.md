# set_n_miss errors appropriately

    Code
      set_n_miss(vec, -1)
    Error <rlang_error>
      `x` must be greater than 0
      `x` is -1

---

    Code
      set_n_miss(vec, "a")
    Error <vctrs_error_cast>
      Can't convert `x` <character> to <integer>.

---

    Code
      set_n_miss(vec, 1.5)
    Error <vctrs_error_cast_lossy>
      Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      set_n_miss(vec, c(1.5, 2))
    Error <rlang_error>
      `x` must be length 1
      `x` is 1.5 and 2, and `x` has length: 2

# set_prop_miss errors appropriately

    Code
      set_prop_miss(vec, -1)
    Error <rlang_error>
      `prop` must be between 0 and 1
      `prop` is -1

---

    Code
      set_prop_miss(vec, "a")
    Error <rlang_error>
      `prop` must be between 0 and 1
      `prop` is a

---

    Code
      set_prop_miss(vec, 1.5)
    Error <rlang_error>
      `prop` must be between 0 and 1
      `prop` is 1.5

---

    Code
      set_prop_miss(vec, c(1.5, 2))
    Error <rlang_error>
      `x` must be length 1
      `x` is 1.5 and 2, and `x` has length: 2

