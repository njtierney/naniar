# ggmissing 0.0.2.9000 (2016/07/29)
=========================

## NEW FEATURE (under development)

- `geom_missing_point()` now works in a way that we expect! Thanks to Miles McBain for working out how to get this to work.


# ggmissing 0.0.1.9000 (2016/07/29)
=========================

## NEW FEATURE (under development)

- tidy summaries for missing data: 
    + `percent_missing_df` returns the percentage of missing data for a data.frame
    + `percent_missing_var` the percentage of variables that contain missing values
    + `percent_missing_case` the percentage of cases that contain missing values.
    + `table_missing_var` table of missing information for variables
    + `table_missing_case` table of missing information for cases
    + `summary_missing_var` summary of missing information for variables (counts, percentages)
    + `summary_missing_case` summary of missing information for variables (counts, percentages)
- gg_missing_col: plot the missingness in each variable
- gg_missing_row: plot the missingness in each case
- gg_missing_which: plot which columns contain missing data.
