# naniar 0.1.5.9000 (2017/01/19)
=========================

# Breaking change

- `cast_shadow` no longer works when called as `cast_shadow(data)`. This action used to return all variables, and then shadow variables for the variables that only contained missing values. This was inconsistent with the use of `cast_shadow(data, var1, var2)`. A new option has been added to `bind_shadow` that controls this - discussed below. See more details at
[issue 65](https://github.com/njtierney/naniar/issues/106).

# New Feature

- `bind_shadow` gains a `only_miss` argument. When set to FALSE (the default) it
will bind a dataframe with all of the variables duplicated with their shadow.
Setting this to TRUE will bind variables only those variables that contain
missing values.

# Bug fix

- `oceanbuoys` now is numeric type for year, latitude, and longitude, previously it was factor. [See related issue](https://github.com/njtierney/naniar/issues/110)

# naniar 0.1.4.9000 (2017/01/12)
=========================

# Breaking change

- Deprecated `replace_to_na`, with `replace_with_na`, as it is a more natural phrase ("replace coffee to tea" vs "replace coffee with tea"). This will be made defunct in the next version.

# New Features

- New "scoped variants" of `replace_with_na`, thankyou to Colin Fay for his work on this:
  - `replace_with_na_all` replaces all NAs across the dataframe that meet a specified condition (using the syntax `~.x == -99`)
  - `replace_with_na_at` replaces all NAs across for specified variables
  - `replace_with_na_if` replaces all NAs for those variables that satisfy some predicate function (e.g., is.character)

# naniar 0.1.3.9100 (2017/12/15)
=========================

# New Features

- Speed improvements. Some discussion with Romain François and Jim Hester have 
  led to some really nice speedups of rowwise counting of missing values. At 
  the moment we have merged Jim's branch of that uses rowMeans, and in the very
  near future will merge in the c++ changes to make naniar as fast as possible.

# New functions

- `n_miss_row` calculates the number of missing values in each row, returning a vector. There are also 3 other functions which are similar in spirit: `n_complete_row`, `prop_miss_row`, and `prop_complete_row`, which return a vector of the number of
complete obserations, the proportion of missings in a row, and the proportion
of complete obserations in a row

- `add_miss_cluster` is a new function that calculates a cluster of missingness 
  for each row, using `hclust`. This can be useful in exploratory modelling
  of missingness, similar to [Tierney et al 2015](http://bmjopen.bmj.com/content/5/6/e007450). and [Barnett et al. 2017](http://bmjopen.bmj.com/content/7/10/e017284)

# Minor Changes

- minor changes to graphics in gg_miss_fct - change legend title from
"Percent Missing" to "% Miss".

# naniar 0.1.2.9000 (2017/09/01)
=========================

# New Features

- Change behaviour of `cast_shadow` so that the default option is to return only the variables that contain missings. This is different to `bind_shadow`, which binds a complete shadow matrix to the dataframe. A way to think about this is that the shadow is only cast on variables that contain missing values, whereas a bind is binding a complete shadow to the data. This may change in the future to be the default option for `bind_shadow`.

# naniar 0.1.1.9000 (2017/08/30)
=========================

# New Features

- Now exported `where_na` - a function that returns the positions of NA values. For a dataframe it returns a matrix of row and col positions of NAs, and for a vector it returns a vector of positions of NAs. (#105)

# Minor Changes

- Update vignettes to have floating menu and better figure size.

# naniar 0.1.0 (2017/08/09) "The Founding of `naniar`"
=========================

- This is the first release of `naniar` onto CRAN, updates to `naniar` will happen reasonably regularly after this approximately every 1-2 months


# naniar 0.0.9.9995 (2017/08/07)
=========================

# Name change
- After careful consideration, I have changed back to `naniar`

# Major Change

- three new functions : `miss_case_cumsum` / `miss_var_cumsum` / `replace_to_na`
- two new visualisations : `gg_var_cumsum` & `gg_case_cumsum`

## New Feature

- `group_by` is now respected by the following functions:
  - `miss_case_cumsum()`
  - `miss_case_summary()`
  - `miss_case_table()`
  - `miss_prop_summary()`
  - `miss_var_cumsum()`
  - `miss_var_run()`
  - `miss_var_span()`
  - `miss_var_summary()`
  - `miss_var_table()`

# Minor changes

- Reviewed documentation for all functions and improved wording, grammar, and 
style.
- Converted roxygen to roxygen markdown
- updated vignettes and readme
- added a new vignette "naniar-visualisation", to give a quick overview of the visualisations provided with naniar.
- changed `label_missing*` to `label_miss` to be more consistent with the rest 
of naniar
- Add `pct` and `prop` helpers (#78)
- removed `miss_df_pct` - this was literally the same as `pct_miss` or `prop_miss`.
- break larger files into smaller, more manageable files (#83)
- `gg_miss_var` gets a `show_pct` argument to show the percentage of missing values (Thanks [Jennifer](https://github.com/jenniferthompson) for the helpful feedback! :))

## Minor changes

- `miss_var_summary` & `miss_case_summary` now have consistent output (one was ordered by n_missing, not the other).
- prevent error in `miss_case_pct`
- `enquo_x` is now `x` (as adviced by [Hadley](https://twitter.com/hadleywickham/status/885993307968593920))
- Now has ByteCompile to TRUE
- add Colin to auth

# narnia 0.0.9.9400 (2017/07/24)
=========================

## new features

- `replace_to_na` is a complement to `tidyr::replace_na` and replaces a specified
value from a variable to NA.
- `gg_miss_fct` returns a heatmap of the number of missings per variable for 
  each level of a factor. This feature was very kindly contributed by 
  [Colin Fay](https://github.com/ColinFay).
- `gg_miss_` functions now return a ggplot object, which behave as such. 
  `gg_miss_` basic themes can be overriden with ggplot functions. This fix
  was very kindly contributed by [Colin Fay](https://github.com/ColinFay).
- removed defunct functions as per #63
- made `add_*` functions handle bare unqouted names where appropriate as per #61
- added tests for the `add_*` family
- got the svgs generated from vdiffr, thanks @karawoo!

## breaking changes

- changed `geom_missing_point()` to `geom_miss_point()`, to keep consistent with the rest of the functions in `naniar`.

# narnia 0.0.8.9100 (2017/06/23)
=========================

## new features

- updated datasets `brfss` and `tao` as per #59

# narnia 0.0.7.9992 (2017/06/22)
=========================

## new features

- `add_label_missings()`
- `add_label_shadow()`
- `cast_shadow()`
- `cast_shadow_shift()`
- `cast_shadow_shift_label()`

- added github issue / contribution / pull request guides
- `ts` generic functions are now `miss_var_span` and `miss_var_run`, and `gg_miss_span` and work on `data.frame`'s, as opposed to just `ts` objects.
- `add_shadow_shift()` adds a column of shadow_shifted values to the current dataframe, adding "_shift" as a suffix
- `cast_shadow()` - acts like `bind_shadow()` but allows for specifying which columns to add
- `shadow_shift` now has a method for factors - powered by `forcats::fct_explicit_na()` #3

## bug fixes

- shadow_shift.numeric works when there is no variance (#37)

## name changes

- changed `is_na` function to `label_na`
- renamed most files to have `tidy-miss-[topic]`
- `gg_missing_*` is changed to `gg_miss_*` to fit with other syntax

## Removed functions

- Removed old functions `miss_cat`, `shadow_df` and `shadow_cat`, as they are no longer needed, and have been superceded by `label_missing_2d`, `as_shadow`, and `is_na`.

## minor changes

- drastically reduced the size of the pedestrian dataset, consider 4 sensor locations, just for 2016.

## New features

- New dataset, `pedestrian` - contains hourly counts of pedestrians
- First pass at time series missing data summaries and plots: 
  - `miss_ts_run()`: return the number of missings / complete in a single run
  - `miss_ts_summary()`: return the number of missings in a given time period
  - `gg_miss_ts()`: plot the number of missings in a given time period

## Name changes

- renamed package from `naniar` to `narnia` - I had to explain the spelling a few times when I was introducing the package and I realised that I should change the name. Fortunately it isn't on CRAN yet.

# naniar 0.0.6.9100 (2017/03/21)
=========================

- Added `prop_miss` and the complement `prop_complete`. Where `n_miss` returns the number of missing values, `prop_miss` returns the proportion of missing values. Likewise, `prop_complete` returns the proportion of complete values. 

## Defunct functions

- As stated in 0.0.5.9000, to address [Issue #38](https://github.com/njtierney/naniar/issues/38), I am moving towards the format miss_type_value/fun, because it makes more sense to me when tabbing through functions.

The left hand side functions have been made defunct in favour of the right hand side.
    - `percent_missing_case()`  --> `miss_case_pct()`
    - `percent_missing_var()`   --> `miss_var_pct()`
    - `percent_missing_df()`    --> `miss_df_pct()`
    - `summary_missing_case()`  --> `miss_case_summary()`
    - `summary_missing_var()`   --> `miss_var_summary()`
    - `table_missing_case()`   --> `miss_case_table()`
    - `table_missing_var()`    --> `miss_var_table()`

# naniar 0.0.5.9000 (2016/01/08)
=========================

## Deprecated functions

- To address [Issue #38](https://github.com/njtierney/naniar/issues/38), I am moving towards the format miss_type_value/fun, because it makes more sense to me when tabbing through functions.
- `miss_*` = I want to explore missing values
- `miss_case_*` = I want to explore missing cases
- `miss_case_pct` = I want to find the percentage of cases containing a missing value
- `miss_case_summary` = I want to find the number / percentage of missings in each case
`miss_case_table` = I want a tabulation of the number / percentage of cases missing

This is more consistent and easier to reason with.

Thus, I have renamed the following functions:
    - `percent_missing_case()`  --> `miss_case_pct()`
    - `percent_missing_var()`   --> `miss_var_pct()`
    - `percent_missing_df()`    --> `miss_df_pct()`
    - `summary_missing_case()`  --> `miss_case_summary()`
    - `summary_missing_var()`   --> `miss_var_summary()`
    - `table_missing_case()`   --> `miss_case_table()`
    - `table_missing_var()`    --> `miss_var_table()`

These will be made defunct in the next release, 0.0.6.9000 ("The Wood Between Worlds"). 

# naniar 0.0.4.9000 (2016/12/31)
=========================

## New features

- `n_complete` is a complement to `n_miss`, and counts the number of complete values in a vector, matrix, or dataframe.

## Bug fixes

- `shadow_shift` now handles cases where there is only 1 complete value in a vector.

## Other changes

- added much more comprehensive testing with `testthat`.

# naniar 0.0.3.9901 (2016/12/18)
=========================

After a burst of effort on this package I have done some refactoring and thought hard about where this package is going to go. This meant that I had to make the decision to rename the package from ggmissing to naniar. The name may strike you as strange but it reflects the fact that there are many changes happening, and that we will be working on creating a nice utopia (like Narnia by CS Lewis) that helps us make it easier to work with missing data

## New Features (under development)

- `add_n_miss` and `add_prop_miss` are helpers that add columns to a dataframe containing the number and proportion of missing values. An example has been provided to use decision trees to explore missing data structure as in [Tierney et al](bmjopen.bmj.com/content/5/6/e007450.full)

- `geom_miss_point()` now supports transparency, thanks to @seasmith (Luke Smith)
 

- more shadows. These are mainly around `bind_shadow` and `gather_shadow`, which are helper functions to assist with creating 

## Bug fixes

- `geom_missing_point()` broke after the new release of ggplot2 2.2.0, but this is now fixed by ensuring that it inherits from GeomPoint, rather than just a new Geom. Thanks to Mitchell O'hara-Wild for his help with this.

- missing data summaries `table_missing_var` and `table_missing_case` also now return more sensible numbers and variable names. It is possible these function names will change in the future, as these are kind of verbose.

- semantic versioning was incorrectly entered in the DESCRIPTION file as  0.2.9000, so I changed it to 0.0.2.9000, and then to 0.0.3.9000 now to indicate the new changes, hopefully this won't come back to bite me later. I think I accidentally did this with visdat at some point as well. Live and learn.

## Other changes

- gathered related functions into single R files rather than leaving them in 
their own.

- correctly imported the `%>%` operator from magrittr, and removed a lot of chaff around `@importFrom` - really don't need to use `@importFrom` that often.

# ggmissing 0.0.2.9000 (2016/07/29)
=========================

## New Feature (under development)

- `geom_missing_point()` now works in a way that we expect! Thanks to Miles McBain for working out how to get this to work.


# ggmissing 0.0.1.9000 (2016/07/29)
=========================

## New Feature (under development)

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
