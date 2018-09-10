# naniar 0.4.0 (2018/09/10) "An Unexpected Meeting"

## New Feature

* Add custom label support for missings and not missings with functions `add_label_missings` and `add_label_shadow()` and `add_any_miss()`. So you can now do `add_label_missings(data, missing = "custom_missing_label", complete = "custom_complete_label")
* `impute_median()` and scoped variants
* `any_shade()` returns a logical TRUE or FALSE depending on if there are any `shade` values
* `nabular()` an alias for `bind_shadow()` to tie the `nabular` term into the work.
* `is_nabular()` checks if input is nabular.

* `geom_miss_point()` now gains the arguments from `shadow_shift()`/`impute_below()` for altering the amount of `jitter` and proportion below (`prop_below`).
* Added two new vignettes, "Exploring Imputed Values", and  "Special Missing Values"
* `miss_var_summary` and `miss_case_summary` now no longer provide the 
cumulative sum of missingness in the summaries - this summary can be added back
to the data with the option `add_cumsum = TRUE`. #186
- Added `gg_miss_upset` to replace workflow of: 
  ```
  data %>% 
    as_shadow_upset() %>%
    UpSetR::upset()
  ```

## Major Change

* `recode_shadow` now works! This function allows you to recode your missing
values into special missing values. These special missing values are stored in
the shadow part of the dataframe, which ends in `_NA`.
* implemented `shade` where appropriate throughout naniar, and also added 
verifiers, `is_shade`, `are_shade`, `which_are_shade`, and removed `which_are_shadow`.
- `as_shadow`  and `bind_shadow` now return data of class `shadow`. This will 
feed into `recode_shadow` methods for flexibly adding new types of missing data.
- Note that in the future `shadow` might be changed to `nabble` or something similar.

## Minor feature

* Functions `add_label_shadow()` and `add_label_missings()` gain arguments so you can only label according to the missingness / shadowy-ness of given variables.
* new function `which_are_shadow()`, to tell you which values are shadows.
* new function `long_shadow()`, which converts data in shadow/nabular form into a long format suitable for plotting. Related to [#165](https://github.com/njtierney/naniar/issues/165)
* Added tests for `miss_scan_count`

## Minor Changes

* `gg_miss_upset` gets a better default presentation by ordering by the largest
intersections, and also an improved error message when data with only 1 or no
variables have missing values.
* `shadow_shift` gains a more informative error message when it doesn't know the class.
* Changed `common_na_string` to include  escape characters for "?", "*", "." so
that if they are used in replacement or searching functions they don't return
the wildcard results from the characters "?", "*", and ".".
* `miss_case_table` and `miss_var_table` now has final column names `pct_vars`,
and `pct_cases` instead of `pct_miss` - fixes #178.

## Breaking Changes

* Deprecated old names of the scalar missingness summaries, in favour of a more
consistent syntax [#171](https://github.com/njtierney/naniar/issues/171). The old the and new are:

|old_names            |new_names            |
|:--------------------|:--------------------|
|`miss_case_pct`      |`pct_miss_case`      |
|`miss_case_prop`     |`prop_miss_case`     |
|`miss_var_pct`       |`pct_miss_var`       |
|`miss_var_prop`      |`prop_miss_var`      |
|`complete_case_pct`  |`pct_complete_case`  |
|`complete_case_prop` |`prop_complete_case` |
|`complete_var_pct`   |`pct_complete_var`   |
|`complete_var_prop`  |`prop_complete_var`  |

These old names will be made defunct in 0.5.0, and removed completely in 0.6.0.

* `impute_below` has changed to be an alias of `shadow_shift` - that is it operates on a single vector. `impute_below_all` operates on all columns in a dataframe (as specified in [#159](https://github.com/njtierney/naniar/issues/159))

## Bug fix

* Ensured that `miss_scan_count` actually `return`'d something.
* `gg_miss_var(airquality)` now prints the ggplot - a typo meant that this did not print the plot

# naniar 0.3.1 (2018/06/10) "Strawberry's Adventure"

## Minor Change

This is a patch release that removes `tidyselect` from the package Imports, as
it is unnecessary. Fixes [#174](https://github.com/njtierney/naniar/issues/174)

# naniar 0.3.0 (2018/06/06) "Digory and his Uncle Are Both in Trouble"
=========================

## New Features

* Added `all_miss()` / `all_na()` equivalent to `all(is.na(x))`
* Added `any_complete()` equivalent to `all(complete.cases(x))`
* Added  `any_miss()` equivalent to `anyNA(x)`
* Added `common_na_numbers` and finalised `common_na_strings` - to provide a 
  list of commonly used NA values 
  [#168](https://github.com/njtierney/naniar/issues/168)
* Added `miss_var_which`, to lists the variable names with missings
* Added `as_shadow_upset` which gets the data into a format suitable for 
  plotting as an `UpSetR` plot:
  
  ```r
  airquality %>%
    as_shadow_upset() %>%
    UpSetR::upset()
  ```
  


* Added some imputation functions to assist with exploring missingness 
  structure and visualisation:
    * `impute_below` Perfoms as for `shadow_shift`, but performs on all columns.
      This means that it imputes missing values 10% below the range of the 
      data (powered by `shadow_shift`), to facilitate graphical exloration of 
      the data. Closes [#145](https://github.com/njtierney/naniar/issues/145)
      There are also scoped variants that work for specific named columns: 
      `impute_below_at`, and for columns that satisfy some predicate function:
      `impute_below_if`.
    * `impute_mean`, imputes the mean value, and scoped variants 
      `impute_mean_at`, and `impute_mean_if`.

* `impute_below` and `shadow_shift` gain arguments `prop_below` and `jitter` 
  to control the degree of shift, and also the extent of jitter.

* Added `complete_{case/var}_{pct/prop}`, which complement
  `miss_{var/case}_{pct/prop}` 
  [#150](https://github.com/njtierney/naniar/issues/150)
* Added `unbind_shadow` and `unbind_data` as helpers to remove shadow columns 
  from data, and data from shadows, respectively.

* Added `is_shadow` and `are_shadow` to determine if something contains a
  shadow column. simimlar to `rlang::is_na` and `rland::are_na`, `is_shadow` 
  this returns a logical vector of length 1, and `are_shadow` returns a logical
  vector of length of the number of names of a data.frame. This might be
  revisited at a later point (see `any_shade` in `add_label_shadow`).

* Aesthetics now map as expected in geom_miss_point(). This means you can write 
  things like `geom_miss_point(aes(colour = Month))` and it works appropriately. 
  Fixed by [Luke Smith](https://github.com/seasmith) in Pull request
  [#144](https://github.com/njtierney/naniar/pull/144), fixing 
  [#137](https://github.com/njtierney/naniar/issues/137).


## Minor Changes

* `miss_var_summary` and `miss_case_summary` now return use `order = TRUE` by
 default, so cases and variables with the most missings are presented in 
 descending order. Fixes [#163](https://github.com/njtierney/naniar/issues/163)

* Changes for Visualisation:
  * Changed the default colours used in `gg_miss_case` and `gg_miss_var` to 
    lorikeet purple (from ochRe package: https://github.com/ropenscilabs/ochRe)
  * `gg_miss_case`
    * The y axis label is now ...
    * Default presentation is with `order_cases = TRUE`.
    * Gains a `show_pct` option to be consistent with `gg_miss_var` 
    [#153](https://github.com/njtierney/naniar/issues/153)
  * `gg_miss_which` is rotated 90 degrees so it is easier to read variable names
  * `gg_miss_fct` uses a minimal theme and tilts the axis labels
    [#118](https://github.com/njtierney/naniar/issues/118).
    
* imported `is_na` and `are_na` from `rlang`.
* Added `common_na_strings`, a list of common `NA` values 
  [#168](https://github.com/njtierney/naniar/issues/168).
* Added some detail on alternative methods for replacing with NA in the 
  vignette "replacing values with NA".


# naniar 0.2.0 (2018/02/08) ("The First Joke and Other Matters")
=========================

## New Features

- Speed improvements. Thanks to the help, contributions, and discussion with 
  Romain François and Jim Hester, naniar now has greatly improved speed for 
  calculating the missingness in each row. These speedups should continue to 
  improve in future releases.

- New "scoped variants" of `replace_with_na`, thankyou to Colin Fay for his 
  work on this:
  - `replace_with_na_all` replaces all NAs across the dataframe that meet a 
  specified condition (using the syntax `~.x == -99`)
  - `replace_with_na_at` replaces all NAs across for specified variables
  - `replace_with_na_if` replaces all NAs for those variables that satisfy some 
  predicate function (e.g., is.character)
- added `which_na` - replacement for `which(is.na(x))`

- `miss_scan_count`. This makes it easier for users to search for particular
  occurrences of these values across their variables. 
  [#119](https://github.com/njtierney/naniar/issues/119)

- `n_miss_row` calculates the number of missing values in each row, returning a
vector. There are also 3 other functions which are similar in spirit:
`n_complete_row`, `prop_miss_row`, and `prop_complete_row`, which return a
vector of the number of complete obserations, the proportion of missings in a
row, and the proportion of complete obserations in a row

- `add_miss_cluster` is a new function that calculates a cluster of missingness 
  for each row, using `hclust`. This can be useful in exploratory modelling
  of missingness, similar to 
  [Tierney et al 2015](http://bmjopen.bmj.com/content/5/6/e007450). and 
  [Barnett et al. 2017](http://bmjopen.bmj.com/content/7/10/e017284)

- Now exported `where_na` - a function that returns the positions of NA values. 
  For a dataframe it returns a matrix of row and col positions of NAs, and for 
  a vector it returns a vector of positions of NAs. (#105)

## Minor changes

- Updated the vignette "Gallery of Missing Data Visualisations" to include 
  the `facet` features and `order_cases`.
- `bind_shadow` gains a `only_miss` argument. When set to FALSE (the default) it
  will bind a dataframe with all of the variables duplicated with their shadow.
  Setting this to TRUE will bind variables only those variables that contain
  missing values.
- Cleaned up the visualisation of `gg_miss_case` to be clearer and less 
  cluttered ( [#117](https://github.com/njtierney/naniar/issues/117)), also 
  added n `order_cases` option to order by cases.
- Added a `facet` argument to `gg_miss_var`, `gg_miss_case`, and 
  `gg_miss_span`. This makes it easier for users to visualise these plots 
  across the values of another variable. In the future I will consider adding 
  `facet` to the other shorthand plotting function, but at the moment these 
  seemed to be the ones that would benefit the most from this feature.

## Bug fix

- `oceanbuoys` now is numeric type for year, latitude, and longitude, 
  previously it was factor. 
  [See related issue](https://github.com/njtierney/naniar/issues/110)
- Improved handling of `shadow_shift` when there are Inf or -Inf values (see  [#117](https://github.com/njtierney/naniar/issues/90))

## Breaking change

- Deprecated `replace_to_na`, with `replace_with_na`, as it is a more natural 
  phrase ("replace coffee to tea" vs "replace coffee with tea"). This will be 
  made defunct in the next version.

- `cast_shadow` no longer works when called as `cast_shadow(data)`. This 
  action used to return all variables, and then shadow variables for the 
  variables that only contained missing values. This was inconsistent with 
  the use of `cast_shadow(data, var1, var2)`. A new option has been added 
  to `bind_shadow` that controls this - discussed below. See more details at
  [issue 65](https://github.com/njtierney/naniar/issues/106).


- Change behaviour of `cast_shadow` so that the default option is to return 
  only the variables that contain missings. This is different to `bind_shadow`, 
  which binds a complete shadow matrix to the dataframe. A way to think about 
  this is that the shadow is only cast on variables that contain missing values,
  whereas a bind is binding a complete shadow to the data. This may change in 
  the future to be the default option for `bind_shadow`.

## Minor Changes

- Update vignettes to have floating menu and better figure size.
- minor changes to graphics in gg_miss_fct - change legend title from 
  "Percent Missing" to "% Miss".


# naniar 0.1.0 (2017/08/09) "The Founding of `naniar`"
=========================

- This is the first release of `naniar` onto CRAN, updates to `naniar` will 
  happen reasonably regularly after this approximately every 1-2 months


# naniar 0.0.9.9995 (2017/08/07)
=========================

## Name change
- After careful consideration, I have changed back to `naniar`

## Major Change

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

## Minor changes

- Reviewed documentation for all functions and improved wording, grammar, and 
style.
- Converted roxygen to roxygen markdown
- updated vignettes and readme
- added a new vignette "naniar-visualisation", to give a quick overview of the 
  visualisations provided with naniar.
- changed `label_missing*` to `label_miss` to be more consistent with the rest 
  of naniar
- Add `pct` and `prop` helpers (#78)
- removed `miss_df_pct` - this was literally the same as `pct_miss` or `prop_miss`.
- break larger files into smaller, more manageable files (#83)
- `gg_miss_var` gets a `show_pct` argument to show the percentage of missing 
  values (Thanks [Jennifer](https://github.com/jenniferthompson) for the 
  helpful feedback! :))

## Minor changes

- `miss_var_summary` & `miss_case_summary` now have consistent output (one was 
  ordered by n_missing, not the other).
- prevent error in `miss_case_pct`
- `enquo_x` is now `x` (as adviced by 
  [Hadley](https://twitter.com/hadleywickham/status/885993307968593920))
- Now has ByteCompile to TRUE
- add Colin to auth

# narnia 0.0.9.9400 (2017/07/24)
=========================

## new features

- `replace_to_na` is a complement to `tidyr::replace_na` and replaces a 
  specified value from a variable to NA.
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

- changed `geom_missing_point()` to `geom_miss_point()`, to keep consistent 
  with the rest of the functions in `naniar`.

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
- `ts` generic functions are now `miss_var_span` and `miss_var_run`, and 
  `gg_miss_span` and work on `data.frame`'s, as opposed to just `ts` objects.
- `add_shadow_shift()` adds a column of shadow_shifted values to the current 
  dataframe, adding "_shift" as a suffix
- `cast_shadow()` - acts like `bind_shadow()` but allows for specifying which 
  columns to add
- `shadow_shift` now has a method for factors - powered by 
  `forcats::fct_explicit_na()` #3

## bug fixes

- shadow_shift.numeric works when there is no variance (#37)

## name changes

- changed `is_na` function to `label_na`
- renamed most files to have `tidy-miss-[topic]`
- `gg_missing_*` is changed to `gg_miss_*` to fit with other syntax

## Removed functions

- Removed old functions `miss_cat`, `shadow_df` and `shadow_cat`, as they are 
  no longer needed, and have been superceded by `label_missing_2d`, 
  `as_shadow`, and `is_na`.

## minor changes

- drastically reduced the size of the pedestrian dataset, consider 4 sensor 
  locations, just for 2016.

## New features

- New dataset, `pedestrian` - contains hourly counts of pedestrians
- First pass at time series missing data summaries and plots: 
  - `miss_ts_run()`: return the number of missings / complete in a single run
  - `miss_ts_summary()`: return the number of missings in a given time period
  - `gg_miss_ts()`: plot the number of missings in a given time period

## Name changes

- renamed package from `naniar` to `narnia` - I had to explain the spelling a 
  few times when I was introducing the package and I realised that I should 
  change the name. Fortunately it isn't on CRAN yet.

# naniar 0.0.6.9100 (2017/03/21)
=========================

- Added `prop_miss` and the complement `prop_complete`. Where `n_miss` returns 
  the number of missing values, `prop_miss` returns the proportion of missing 
  values. Likewise, `prop_complete` returns the proportion of complete values. 

## Defunct functions

- As stated in 0.0.5.9000, to address 
  [Issue #38](https://github.com/njtierney/naniar/issues/38), I am moving 
  towards the format miss_type_value/fun, because it makes more sense to me 
  when tabbing through functions.

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

- To address [Issue #38](https://github.com/njtierney/naniar/issues/38), I am 
  moving towards the format miss_type_value/fun, because it makes more sense 
  to me when tabbing through functions.
- `miss_*` = I want to explore missing values
- `miss_case_*` = I want to explore missing cases
- `miss_case_pct` = I want to find the percentage of cases containing a 
  missing value
- `miss_case_summary` = I want to find the number / percentage of missings in 
  each case
- `miss_case_table` = I want a tabulation of the number / percentage of cases 
  missing

This is more consistent and easier to reason with.

Thus, I have renamed the following functions:
    - `percent_missing_case()`  --> `miss_case_pct()`
    - `percent_missing_var()`   --> `miss_var_pct()`
    - `percent_missing_df()`    --> `miss_df_pct()`
    - `summary_missing_case()`  --> `miss_case_summary()`
    - `summary_missing_var()`   --> `miss_var_summary()`
    - `table_missing_case()`   --> `miss_case_table()`
    - `table_missing_var()`    --> `miss_var_table()`

These will be made defunct in the next release, 0.0.6.9000 
("The Wood Between Worlds"). 

# naniar 0.0.4.9000 (2016/12/31)
=========================

## New features

- `n_complete` is a complement to `n_miss`, and counts the number of complete 
  values in a vector, matrix, or dataframe.

## Bug fixes

- `shadow_shift` now handles cases where there is only 1 complete value in a vector.

## Other changes

- added much more comprehensive testing with `testthat`.

# naniar 0.0.3.9901 (2016/12/18)
=========================

After a burst of effort on this package I have done some refactoring and thought
hard about where this package is going to go. This meant that I had to make the
decision to rename the package from ggmissing to naniar. The name may strike you
as strange but it reflects the fact that there are many changes happening, and
that we will be working on creating a nice utopia (like Narnia by CS Lewis) that
helps us make it easier to work with missing data

## New Features (under development)

- `add_n_miss` and `add_prop_miss` are helpers that add columns to a dataframe
containing the number and proportion of missing values. An example has been
provided to use decision trees to explore missing data structure as in 
[Tierney et al](bmjopen.bmj.com/content/5/6/e007450.full)

- `geom_miss_point()` now supports transparency, thanks to @seasmith (Luke Smith)
 

- more shadows. These are mainly around `bind_shadow` and `gather_shadow`, 
which are helper functions to assist with creating 

## Bug fixes

- `geom_missing_point()` broke after the new release of ggplot2 2.2.0, but this
is now fixed by ensuring that it inherits from GeomPoint, rather than just a new
Geom. Thanks to Mitchell O'hara-Wild for his help with this.

- missing data summaries `table_missing_var` and `table_missing_case` also now
return more sensible numbers and variable names. It is possible these function
names will change in the future, as these are kind of verbose.

- semantic versioning was incorrectly entered in the DESCRIPTION file as
0.2.9000, so I changed it to 0.0.2.9000, and then to 0.0.3.9000 now to indicate
the new changes, hopefully this won't come back to bite me later. I think I
accidentally did this with visdat at some point as well. Live and learn.

## Other changes

- gathered related functions into single R files rather than leaving them in 
their own.

- correctly imported the `%>%` operator from magrittr, and removed a lot of
chaff around `@importFrom` - really don't need to use `@importFrom` that often.

# ggmissing 0.0.2.9000 (2016/07/29)
=========================

## New Feature (under development)

- `geom_missing_point()` now works in a way that we expect! Thanks to Miles
McBain for working out how to get this to work.


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
