
<!-- README.md is generated from README.Rmd. Please edit that file -->
naniar
======

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/njtierney/naniar?branch=master&svg=true)](https://ci.appveyor.com/project/njtierney/naniar) [![Travis-CI Build Status](https://travis-ci.org/njtierney/naniar.svg?branch=master)](https://travis-ci.org/njtierney/naniar) [![Coverage Status](https://img.shields.io/codecov/c/github/njtierney/naniar/master.svg)](https://codecov.io/github/njtierney/naniar?branch=master)

`naniar` aims to make it easy to summarise, visualise, and manipulate missing data with minimal deviations from the workflows in ggplot2 and tidy data.

Currently it provides:

-   Data structures for missing data, e.g., `bind_shadow()`
-   Handy shorthand summaries for missing data: e.g., `n_miss()`, `n_complete`, `prop_miss()`, `prop_complete`.
-   Numerical summaries of missing data: e.g., `miss_case_summary()`, `miss_case_table()`, `miss_var_pct()`, `miss_var_summary()`, `miss_var_run`.
-   Visualisation methods: e.g., `geom_miss_point()`, `gg_miss_var()`,

For details on all of these functions and how to use each of them, you can read the vignette ["Getting Started with naniar"](http://naniar.njtierney.com/articles/getting-started-w-naniar.html).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Data structures for missing data
================================

Representing missing data structure is achieved using the shadow matrix, introduced in [Swayne and Buja](https://www.researchgate.net/publication/2758672_Missing_Data_in_Interactive_High-Dimensional_Data_Visualization). The shadow matrix is the same dimension as the data, and consists of binary indicators of missingness of data values, where missing is represented as "NA", and not missing is represented as "!NA". Although these may be represented as 1 and 0, respectively. This representation can be seen in the figure below, adding the suffix "\_NA" to the variables. This structure can also be extended to allow for additional factor levels to be created. For example 0 indicates data presence, 1 indicates missing values, 2 indicates imputed value, and 3 might indicate a particular type or class of missingness, where reasons for missingness might be known or inferred. The data matrix can also be augmented to include the shadow matrix, which facilitates visualisation of univariate and bivariate missing data visualisations. Another format is to display it in long form, which facilitates heatmap style visualisations. This approach can be very helpful for giving an overview of which variables contain the most missingness. Methods can also be applied to rearrange rows and columns to find clusters, and identify other interesting features of the data that may have previously been hidden or unclear.

<img src="missingness-data-structures.png" width="800px" />

**Illustration of data structures for facilitating visualisation of missings and not missings**

Visualising missing data
========================

Visualising missing data might sound a little strange - how do you visualise something that is not there? One approach to visualising missing data comes from ggobi and manet, where we replace "NA" values with values 10% lower than the minimum value in that variable. This is provided with the `geom_miss_point()` ggplot2 geom, which we can illustrate by exploring the relationship between Ozone and Solar radiation from the airquality dataset.

``` r

library(ggplot2)

ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_point()
#> Warning: Removed 42 rows containing missing values (geom_point).
```

![](README-figs/README-regular-geom-point-1.png)

ggplot2 does not handle these missing values, and we get a warning message about the missing values.

We can instead use the `geom_miss_point()` to display the missing data

``` r

library(naniar)

ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_miss_point()
```

![](README-figs/README-geom-missing-point-1.png)

`geom_miss_point()` has shifted the missing values to now be 10% below the minimum value. The missing values are a different colour so that missingness becomes pre-attentive.

This plays nicely with other parts of ggplot, like adding transparency

``` r

ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_miss_point(alpha = 0.5)
```

![](README-figs/README-geom-missing-point-alpha-1.png)

Thanks to Luke Smith for making this pull request.

We can also add features such as faceting, just like any regular ggplot plot.

For example, we can split the facet by month:

``` r

p1 <-
ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) + 
  geom_miss_point() + 
  facet_wrap(~Month, ncol = 2) + 
  theme(legend.position = "bottom")

p1
```

![](README-figs/README-facet-by-month-1.png)

And then change the theme, just like you do with any other ggplot graphic

``` r

p1 + theme_bw()  
```

![](README-figs/README-facet-add-theme-1.png)

You can also look at the proportion of missings in each variable with gg\_miss\_var:

``` r

gg_miss_var(airquality)
```

![](README-figs/README-gg-missing-var-1.png)

The plots created with the `gg_miss` family all have a basic theme, but you can customise them, just like usual ggplot objects. If you call any ggplot customisation functions with a `gg_miss` object, the default args will be overridden.

``` r
gg_miss_var(airquality) + theme_bw() 
```

![](README-figs/README-unnamed-chunk-3-1.png)

``` r

gg_miss_var(airquality) + labs(y = "Look at all the missing ones")
```

![](README-figs/README-unnamed-chunk-3-2.png)

You can also explore the whole dataset of missings using the `vis_miss` function, which is exported from the [`visdat`](github.com/ropensci/visdat) package.

``` r

vis_miss(airquality)
```

![](README-figs/README-viss-miss-1.png)

Another approach can be to use **Univariate plots split by missingness**. We can do this using the `bind_shadow()` argument to place the data and shadow side by side. This allows for us to examine univariate distributions according to the presence or absence of another variable.

``` r

aq_shadow <- bind_shadow(airquality)

aq_shadow
#> # A tibble: 153 x 12
#>    Ozone Solar.R  Wind  Temp Month   Day Ozone_NA Solar.R_NA Wind_NA
#>    <int>   <int> <dbl> <int> <int> <int>   <fctr>     <fctr>  <fctr>
#>  1    41     190   7.4    67     5     1      !NA        !NA     !NA
#>  2    36     118   8.0    72     5     2      !NA        !NA     !NA
#>  3    12     149  12.6    74     5     3      !NA        !NA     !NA
#>  4    18     313  11.5    62     5     4      !NA        !NA     !NA
#>  5    NA      NA  14.3    56     5     5       NA         NA     !NA
#>  6    28      NA  14.9    66     5     6      !NA         NA     !NA
#>  7    23     299   8.6    65     5     7      !NA        !NA     !NA
#>  8    19      99  13.8    59     5     8      !NA        !NA     !NA
#>  9     8      19  20.1    61     5     9      !NA        !NA     !NA
#> 10    NA     194   8.6    69     5    10       NA        !NA     !NA
#> # ... with 143 more rows, and 3 more variables: Temp_NA <fctr>,
#> #   Month_NA <fctr>, Day_NA <fctr>
```

The plot below shows the values of temperature when ozone is present and missing, on the left is a faceted histogram, and on the right is an overlaid density.

``` r

library(ggplot2)

p1 <- ggplot(data = aq_shadow,
       aes(x = Temp)) + 
  geom_histogram() + 
  facet_wrap(~Ozone_NA,
             ncol = 1)

p2 <- ggplot(data = aq_shadow,
       aes(x = Temp,
           colour = Ozone_NA)) + 
  geom_density() 

gridExtra::grid.arrange(p1, p2, ncol = 2)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README-figs/README-bind-shadow-density-1.png)

The other plotting functions can be seen in the vignette, "Gallery of missing data visualisations with naniar".

Handy helpers for missing data
==============================

Users may be aware that you can find the number of missings in a structure using `sum(is.na(x))`, or find the proportion of missings with `mean(is.na(x))`. to make this is a bit easier to think about and reason with, we introduce more intuitive helpers:

-   `n_miss()` / `n_complete()` return the number of missings or complete values
-   `prop_miss()` / `prop_complete()` return the proportion of missing or complete values.
-   `pct_miss()` / `pct_complete()` likewise return the percentage of missing or complete values.

Numerical summaries for missing data
====================================

`naniar` provides numerical summaries of missing data starting with `miss_`. Summaries focussing on variables or a single selected variable, start with `miss_var_`, and summaries for cases (the initial collected row order of the data), they start with `miss_case_`. All of these functions also "just work" with dplyr's `group_by()`.

For example, we can look at the number and percent of missings in each case and variable with `miss_var_summary()`, and `miss_case_summary()`, which both return output ordered by the number of missing values.

``` r

miss_var_summary(airquality)
#> # A tibble: 6 x 3
#>   variable n_missing   percent
#>      <chr>     <int>     <dbl>
#> 1    Ozone        37 24.183007
#> 2  Solar.R         7  4.575163
#> 3     Wind         0  0.000000
#> 4     Temp         0  0.000000
#> 5    Month         0  0.000000
#> 6      Day         0  0.000000
miss_case_summary(airquality)
#> # A tibble: 153 x 3
#>     case n_missing  percent
#>    <int>     <int>    <dbl>
#>  1     5         2 33.33333
#>  2    27         2 33.33333
#>  3     6         1 16.66667
#>  4    10         1 16.66667
#>  5    11         1 16.66667
#>  6    25         1 16.66667
#>  7    26         1 16.66667
#>  8    32         1 16.66667
#>  9    33         1 16.66667
#> 10    34         1 16.66667
#> # ... with 143 more rows
```

You could also `group_by()` to work out the number of missings in each variable across the levels within it.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
airquality %>%
  group_by(Month) %>%
  miss_var_summary()
#> # A tibble: 25 x 4
#>    Month variable n_missing  percent
#>    <int>    <chr>     <int>    <dbl>
#>  1     5    Ozone         5 16.12903
#>  2     5  Solar.R         4 12.90323
#>  3     5     Wind         0  0.00000
#>  4     5     Temp         0  0.00000
#>  5     5      Day         0  0.00000
#>  6     6    Ozone        21 70.00000
#>  7     6  Solar.R         0  0.00000
#>  8     6     Wind         0  0.00000
#>  9     6     Temp         0  0.00000
#> 10     6      Day         0  0.00000
#> # ... with 15 more rows
```

To find the percent missing variables and cases:

``` r

# Proportion of variables that contain any missing values
miss_var_pct(airquality)
#> [1] 33.33333
 # Proportion of cases that contain any missing values
miss_case_pct(airquality)
#> [1] 27.45098
# Proportion elements in dataset that contains missing values
prop_miss(airquality)
#> [1] 0.04793028
```

Tabulations of the number of missings in each case or variable can be calculated with `miss_var_table()` and `miss_case_table()`.

``` r

miss_var_table(airquality)
#> # A tibble: 3 x 3
#>   n_missing_in_var n_vars  percent
#>              <int>  <int>    <dbl>
#> 1                0      4 66.66667
#> 2                7      1 16.66667
#> 3               37      1 16.66667
miss_case_table(airquality)
#> # A tibble: 3 x 3
#>   n_missing_in_case n_cases  percent
#>               <int>   <int>    <dbl>
#> 1                 0     111 72.54902
#> 2                 1      40 26.14379
#> 3                 2       2  1.30719
```

Most of these functions can be called at once using `miss_summary()`, which takes a data.frame and then returns a nested dataframe containing the percentages of missing data, and lists of dataframes containing tally and summary information for the variables and cases.

``` r

s_miss <- miss_summary(airquality)

s_miss
#> # A tibble: 1 x 9
#>   miss_df_prop miss_var_prop miss_case_prop  miss_case_table
#>          <dbl>         <dbl>          <dbl>           <list>
#> 1   0.04793028     0.3333333      0.2745098 <tibble [3 x 3]>
#> # ... with 5 more variables: miss_var_table <list>,
#> #   miss_var_summary <list>, miss_case_summary <list>,
#> #   miss_var_cumsum <list>, miss_case_cumsum <list>

# overall % missing data
s_miss$percent_missing_df
#> Warning: Unknown or uninitialised column: 'percent_missing_df'.
#> NULL

# % of variables that contain missing data
s_miss$percent_missing_var
#> Warning: Unknown or uninitialised column: 'percent_missing_var'.
#> NULL

# % of cases that contain missing data
s_miss$percent_missing_case
#> Warning: Unknown or uninitialised column: 'percent_missing_case'.
#> NULL

# tabulations of missing data across cases
s_miss$table_missing_case
#> Warning: Unknown or uninitialised column: 'table_missing_case'.
#> NULL

# tabulations of missing data across variables
s_miss$table_missing_var
#> Warning: Unknown or uninitialised column: 'table_missing_var'.
#> NULL

# summary information (counts, percentrages) of missing data for variables and cases
s_miss$summary_missing_var
#> Warning: Unknown or uninitialised column: 'summary_missing_var'.
#> NULL
s_miss$summary_missing_case
#> Warning: Unknown or uninitialised column: 'summary_missing_case'.
#> NULL
```

Other plotting functions
========================

gg\_miss\_var
-------------

Display the amount of missing data for each variable

``` r

gg_miss_var(airquality)
```

![](README-figs/README-unnamed-chunk-4-1.png)

gg\_miss\_case
--------------

Display the amount of missing data for each case

``` r

gg_miss_case(airquality)
```

![](README-figs/README-unnamed-chunk-5-1.png)

``` r

gg_miss_which(airquality)
```

![](README-figs/README-unnamed-chunk-6-1.png)

gg\_miss\_case\_cumsum
----------------------

This shows a lineplot of the cumulative sum of missing values for the cases, kindly contributed by Colin Fay.

``` r

gg_miss_case_cumsum(airquality)
```

![](README-figs/README-gg-miss-case-cumsum-1.png)

gg\_miss\_var\_cumsum
---------------------

This shows a lineplot of the cumulative sum of missing values for variables, kindly contributed by Colin Fay.

``` r

gg_miss_var_cumsum(airquality)
```

![](README-figs/README-gg-miss-var-cumsum-1.png)

gg\_miss\_which
---------------

This shows whether a given variable contains a missing variable. In this case grey = missing. Think of it as if you are shading the cell in, if it contains data.

A note on the name
==================

**Why `naniar`?**

`naniar` was previously named `ggmissing` and initially provided a ggplot geom and some other visualisations. `ggmissing` was changed to `naniar` to reflect the fact that this package is going to be bigger in scope, and is not just related to ggplot2. Specifically, the package is designed to provide a suite of tools for generating visualisations of missing values and imputations, manipulate, and summarise missing data.

> ...But *why* `naniar`?

Well, I think it is useful to think of missing values in data being like this other dimension, perhaps like [C.S. Lewis's Narnia](https://en.wikipedia.org/wiki/The_Chronicles_of_naniar) - a different world, hidden away. You go inside, and sometimes it seems like you've spent no time in there but time has passed very quickly, or the opposite. Also, `NA`niar = na in r, and if you so desire, naniar may sound like "noneoya" in an nz/aussie accent. Full credit to @MilesMcbain for the name, and @Hadley for the rearranged spelling.

Future Work
===========

Other plans to extend the `geom_miss_` family to include:

-   Categorical variables
-   Bivariate plots: scatterplots, density overlays
-   SQL translation for databases
-   Big Data tools (sparklyr, sparklingwater)
-   Work well with other imputation engines / processes
-   Provide tools for assessing goodness of fit for classical approaches of MCAR, MAR, and MNAR (graphical inference from `nullabor` package)
-   Expand ggplot `geom_miss_*` family

Acknowledgements
----------------

Firstly, thanks to @dicook for giving the initial inspiration for the package and laying down the rich theory and literature that the work in `naniar` is built upon. Naming credit (once again!) goes to @MilesMcBain. Among various other things, Miles also worked out how to overload the missing data and make it work as a geom. Thanks also to Colin Fay @colinfay for helping me understand tidy evaluation and for features such as `replace_to_na`, `miss_*_cumsum`, and more.
