# nanar 0.0.3.9901 (2016/12/18)

## NEW FEATURES

* `add_n_miss` and `add_prop_miss` are helpers that add columns to a dataframe containing the number and proportion of missing values. An example has been provided to use decision trees to explore missing data structure as in [Tierney et al](bmjopen.bmj.com/content/5/6/e007450.full)

* `geom_miss_point()` now supports transparency, thanks to @seasmith (Luke Smith)
 
# nanar 0.0.3.9000 (2016/12/18)

After a burst of effort on this package I have done some refactoring and thought hard about where this package is going to go. This meant that I had to make the decision to rename the package from ggmissing to naniar. The name may strike you as strange but it reflects the fact that there are many changes happening, and that we will be working on creating a nice utopia (like Narnia by CS Lewis) that helps us make it easier to work with missing data

## NEW FEATURES (under development)

* more shadows. These are mainly around `bind_shadow` and `gather_shadow`, which are helper functions to assist with creating 

## BUG FIXES

* `geom_missing_point()` broke after the new release of ggplot2 2.2.0, but this is now fixed by ensuring that it inherits from GeomPoint, rather than just a new Geom. Thanks to Mitchell O'hara-Wild for his help with this.

* missing data summaries `table_missing_var` and `table_missing_case` also now return more sensible numbers and variable names. It is possible these function names will change in the future, as these are kind of verbose.

* semantic versioning was incorrectly entered in the DESCRIPTION file as  0.2.9000, so I changed it to 0.0.2.9000, and then to 0.0.3.9000 now to indicate the new changes, hopefully this won't come back to bite me later. I think I accidentally did this with visdat at some point as well. Live and learn.

## Other changes

* gathered related functions into single R files rather than leaving them in 
their own.

* correctly imported the `%>%` operator from magrittr, and removed a lot of chaff around `@importFrom` - really don't need to use `@importFrom` that often.

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
