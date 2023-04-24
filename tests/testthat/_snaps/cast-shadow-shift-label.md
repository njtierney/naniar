# cast_shadow_shift_label returns nice error if variables aren't included

    Code
      cast_shadow_shift(airquality)
    Warning <lifecycle_warning_deprecated>
      The `value` argument of `names<-` must have the same length as `x` as of tibble 3.0.0.
    Error <vctrs_error_incompatible_size>
      Can't recycle `..1` (size 153) to match `..2` (size 0).

