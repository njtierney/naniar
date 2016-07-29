
<!-- README.md is generated from README.Rmd. Please edit that file -->
ggmissing
=========

ggplot does not display missing data and currently provides a warning message and omits the missing data.`ggmissing` aims to enable ggplot to display missingness.

`ggmissing` is part of a larger plan for a set of tidy-verse packages focussing on how to tidy, transform, visualise, model, and communicate missing data.

It is still very much under development, but will see more active development over the next 6 months.

What does it do?
----------------

Plotting missing data might sound a little strange - how do you visualise something that is not there? In the past, GGobi and Manet have provided methods of visualising missingness, with one approach being to replace "NA" values with values 10% lower than the minimum value in that variable.

To illustrate, let's explore the relationship between Ozone and Solar radiation from the airquality dataset.

``` r
library(ggmissing)
# devtools::install_github("njtierney/ggmissing")
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

``` r

ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_point()
#> Warning: Removed 42 rows containing missing values (geom_point).
```

![](README-unnamed-chunk-3-1.png)

We get a little message, warning us about the missing values.

We can instead use the `geom_missing_point` to display the missing data

``` r

library(ggmissing)

ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_missing_point()
```

![](README-unnamed-chunk-4-1.png)

`geom_missing_point()` has (albeit somewhat clumsily) shifted the missing values to now be 10% below the minimum value. However, the missing values need to be shown in a different colour so that missingness becomes preattentive. One way to do this is to write the code as follows

``` r
  airquality %>%
  mutate(miss_cat = miss_cat(., "Ozone", "Solar.R")) %>%
  ggplot(data = .,
       aes(x = shadow_shift(Ozone),
           y = shadow_shift(Solar.R),
           colour = miss_cat)) + 
  geom_point() 
```

![](README-unnamed-chunk-5-1.png)

However, this is a little verbose.

We've worked out how to override ggplot removing the missing data, but are still working on automatically setting the colour of missing and non-missing

Examples of the current work, in a stream-of-consciousness style can be seen in the vignette.

The functions in this package are basically utility functions, `shadow_shift`, which shifts missing values to 10% below minimum, `shadow_df` creates a shadow matrix, `miss_cat` creates a new column of missingness status that allows for the plot to create a factor out of missingness. `miss_cat` uses the utility function `shadow_cat`.

Missing data tidying functions
==============================

`ggmissing` uses some missingness transformation functions to set up tables for visualisation.

``` r

# overall percentage of missing data
percent_missing_df(airquality)
#> [1] 4.793028

# % of variables that contain missing data
percent_missing_var(airquality)
#> [1] 33.33333

# % of cases that contain missing data
percent_missing_case(airquality)
#> [1] 27.45098

# tabulations of missing data across cases
table_missing_case(airquality)
#> # A tibble: 3 x 3
#>   n_missing_in_case n_missing  percent
#>               <int>     <int>    <dbl>
#> 1                 0       111 72.54902
#> 2                 1        40 26.14379
#> 3                 2         2  1.30719

# tabulations of missing data across variables
table_missing_var(airquality)
#> # A tibble: 3 x 3
#>   n_missing_in_var n_var   percent
#>              <int> <int>     <dbl>
#> 1                0     4 2.6143791
#> 2                7     1 0.6535948
#> 3               37     1 0.6535948

# summary information (counts, percentrages) of missing data for variables and cases
summary_missing_var(airquality)
#> # A tibble: 6 x 3
#>   variables n_missing   percent
#>       <chr>     <int>     <dbl>
#> 1     Ozone        37 24.183007
#> 2   Solar.R         7  4.575163
#> 3      Wind         0  0.000000
#> 4      Temp         0  0.000000
#> 5     Month         0  0.000000
#> 6       Day         0  0.000000
summary_missing_case(airquality)
#> # A tibble: 153 x 3
#>     case n_missing  percent
#>    <int>     <int>    <dbl>
#> 1      1         0  0.00000
#> 2      2         0  0.00000
#> 3      3         0  0.00000
#> 4      4         0  0.00000
#> 5      5         2 33.33333
#> 6      6         1 16.66667
#> 7      7         0  0.00000
#> 8      8         0  0.00000
#> 9      9         0  0.00000
#> 10    10         1 16.66667
#> # ... with 143 more rows
```

Each of these functions can also be called all together using `summarise_missingness`, which takes a `data.frame` and then returns a nested dataframe containing the percentages of missing data, and lists of dataframes containing tally and summary information for the variables and cases.

``` r

s_miss <- summarise_missingness(airquality)

s_miss
#> # A tibble: 1 x 7
#>   percent_missing_df percent_missing_var percent_missing_case
#>                <dbl>               <dbl>                <dbl>
#> 1           4.793028            33.33333             27.45098
#> # ... with 4 more variables: table_missing_case <list>,
#> #   table_missing_var <list>, summary_missing_var <list>,
#> #   summary_missing_case <list>

# overall % missing data
s_miss$percent_missing_df
#> [1] 4.793028

# % of variables that contain missing data
s_miss$percent_missing_var
#> [1] 33.33333

# % of cases that contain missing data
s_miss$percent_missing_case
#> [1] 27.45098

# tabulations of missing data across cases
s_miss$table_missing_case
#> [[1]]
#> # A tibble: 3 x 3
#>   n_missing_in_case n_missing  percent
#>               <int>     <int>    <dbl>
#> 1                 0       111 72.54902
#> 2                 1        40 26.14379
#> 3                 2         2  1.30719

# tabulations of missing data across variables
s_miss$table_missing_var
#> [[1]]
#> # A tibble: 3 x 3
#>   n_missing_in_var n_var   percent
#>              <int> <int>     <dbl>
#> 1                0     4 2.6143791
#> 2                7     1 0.6535948
#> 3               37     1 0.6535948

# summary information (counts, percentrages) of missing data for variables and cases
s_miss$summary_missing_var
#> [[1]]
#> # A tibble: 6 x 3
#>   variables n_missing   percent
#>       <chr>     <int>     <dbl>
#> 1     Ozone        37 24.183007
#> 2   Solar.R         7  4.575163
#> 3      Wind         0  0.000000
#> 4      Temp         0  0.000000
#> 5     Month         0  0.000000
#> 6       Day         0  0.000000
s_miss$summary_missing_case
#> [[1]]
#> # A tibble: 153 x 3
#>     case n_missing  percent
#>    <int>     <int>    <dbl>
#> 1      1         0  0.00000
#> 2      2         0  0.00000
#> 3      3         0  0.00000
#> 4      4         0  0.00000
#> 5      5         2 33.33333
#> 6      6         1 16.66667
#> 7      7         0  0.00000
#> 8      8         0  0.00000
#> 9      9         0  0.00000
#> 10    10         1 16.66667
#> # ... with 143 more rows
```

Other plotting functions
========================

These tables are then used in these plots

gg\_missing\_var
----------------

``` r

gg_missing_var(airquality)
```

![](README-unnamed-chunk-8-1.png)

gg\_missing\_case
-----------------

``` r

gg_missing_case(airquality)
```

![](README-unnamed-chunk-9-1.png)

gg\_missing\_which
------------------

This shows whether

``` r

gg_missing_which(airquality)
```

![](README-unnamed-chunk-10-1.png)

heatmap of missing data?
------------------------

I recommend the use of the `vis_miss` function from the [`visdat`](github.com/njtierney/visdat) package.

``` r

library(visdat)

vis_miss(airquality)
```

![](README-unnamed-chunk-11-1.png)

Future Work
===========

`ggmissing` has not seen much attention for the past 6 months or so, and so will be undergoing more changes over the next 6 months.

We will be considering how to display missing data with other visualisations, such as:

-   1D, univaritae distribution plots
-   Categorical variables
-   Bivariate plots: Scatterplots, Density overlays.
