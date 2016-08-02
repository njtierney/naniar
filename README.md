
<!-- README.md is generated from README.Rmd. Please edit that file -->
ggmissing
=========

`ggmissing` adds ggplot `geom`s to display missingness.

Why?
----

Missing data is ubiquitous in data analysis. Visualising missing data is a non-trivial process, but has been made easier with packages such as VIM, visdat, and extracat.

However, visualising missing data using ggplot2 - which is perhaps now the most widely used visualisation package in R - is currently non trivial, as it omits missing data in bivariate, and univariate plots. `ggmissing` reduces the cognitive load of dealing with missing data in day-to-day analysis by providing missing data geoms for ggplot, tidy missing data summary functions, and other missing data visualisations.

`ggmissing` is part of a larger plan for a set of tidy-verse packages focussing on how to tidy, transform, visualise, model, and communicate missing data.

`ggmissing` is still very much under development, and may have unknown bugs, due to the fact that ggplot was not initially built to handle missing data in this way. We will see more active development over the next 6 months.

What does `ggmissing` do?
-------------------------

`ggmissing` provides:

1.  Missing data geoms for ggplot (`geom_missing_point`)

2.  Tidy summaries of missing data (`summarise_missingness` and friends)

3.  Standard plots for exploring missing data (`gg_missing_var`, `gg_missing_case`, `gg_missing_which`)

Using ggmissing
---------------

### How does it work?

Plotting missing data might sound a little strange - how do you visualise something that is not there? In the past, GGobi and Manet have provided methods of visualising missingness, with one approach being to replace "NA" values with values 10% lower than the minimum value in that variable.

ggmissing uses the `shadow_shift` function to achieve this shift in the missing values

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

We can instead use the `geom_missing_point()` to display the missing data

``` r

library(ggmissing)

ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_missing_point()
```

![](README-unnamed-chunk-4-1.png)

`geom_missing_point()` has shifted the missing values to now be 10% below the minimum value. The missing values are a different colour so that missingness becomes preattentive.

This plays nicely with other parts of ggplot, like faceting - we can split the facet by month:

``` r
p1 <-
ggplot(data = airquality,
       aes(x = Ozone,
           y = Solar.R)) + 
  geom_missing_point() + 
  facet_wrap(~Month, ncol = 2) + 
  theme(legend.position = "bottom")

p1
```

![](README-unnamed-chunk-5-1.png)

And then change the theme, just like you do with any other ggplot graphic

``` r

p1 + theme_bw()  
```

![](README-unnamed-chunk-6-1.png)

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

dplyr::glimpse(s_miss)
#> Observations: 1
#> Variables: 7
#> $ percent_missing_df   <dbl> 4.793028
#> $ percent_missing_var  <dbl> 33.33333
#> $ percent_missing_case <dbl> 27.45098
#> $ table_missing_case   <list> <c("0", "1", "2"), c("111", "40", "2"), ...
#> $ table_missing_var    <list> <c("0", "7", "37"), c("4", "1", "1"), c(...
#> $ summary_missing_var  <list> <c("Ozone", "Solar.R", "Wind", "Temp", "...
#> $ summary_missing_case <list> <c("1", "2", "3", "4", "5", "6", "7", "8...

knitr::kable(head(s_miss))
```

|  percent\_missing\_df|  percent\_missing\_var|  percent\_missing\_case| table\_missing\_case                                                                 | table\_missing\_var                                                                                | summary\_missing\_var                                                                                     | summary\_missing\_case                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|---------------------:|----------------------:|-----------------------:|:-------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|              4.793028|               33.33333|                27.45098| 0.00000, 1.00000, 2.00000, 111.00000, 40.00000, 2.00000, 72.54902, 26.14379, 1.30719 | 0.0000000, 7.0000000, 37.0000000, 4.0000000, 1.0000000, 1.0000000, 2.6143791, 0.6535948, 0.6535948 | Ozone, Solar.R, Wind, Temp, Month, Day, 37, 7, 0, 0, 0, 0, 24.1830065359477, 4.57516339869281, 0, 0, 0, 0 | 1.00000, 2.00000, 3.00000, 4.00000, 5.00000, 6.00000, 7.00000, 8.00000, 9.00000, 10.00000, 11.00000, 12.00000, 13.00000, 14.00000, 15.00000, 16.00000, 17.00000, 18.00000, 19.00000, 20.00000, 21.00000, 22.00000, 23.00000, 24.00000, 25.00000, 26.00000, 27.00000, 28.00000, 29.00000, 30.00000, 31.00000, 32.00000, 33.00000, 34.00000, 35.00000, 36.00000, 37.00000, 38.00000, 39.00000, 40.00000, 41.00000, 42.00000, 43.00000, 44.00000, 45.00000, 46.00000, 47.00000, 48.00000, 49.00000, 50.00000, 51.00000, 52.00000, 53.00000, 54.00000, 55.00000, 56.00000, 57.00000, 58.00000, 59.00000, 60.00000, 61.00000, 62.00000, 63.00000, 64.00000, 65.00000, 66.00000, 67.00000, 68.00000, 69.00000, 70.00000, 71.00000, 72.00000, 73.00000, 74.00000, 75.00000, 76.00000, 77.00000, 78.00000, 79.00000, 80.00000, 81.00000, 82.00000, 83.00000, 84.00000, 85.00000, 86.00000, 87.00000, 88.00000, 89.00000, 90.00000, 91.00000, 92.00000, 93.00000, 94.00000, 95.00000, 96.00000, 97.00000, 98.00000, 99.00000, 100.00000, 101.00000, 102.00000, 103.00000, 104.00000, 105.00000, 106.00000, 107.00000, 108.00000, 109.00000, 110.00000, 111.00000, 112.00000, 113.00000, 114.00000, 115.00000, 116.00000, 117.00000, 118.00000, 119.00000, 120.00000, 121.00000, 122.00000, 123.00000, 124.00000, 125.00000, 126.00000, 127.00000, 128.00000, 129.00000, 130.00000, 131.00000, 132.00000, 133.00000, 134.00000, 135.00000, 136.00000, 137.00000, 138.00000, 139.00000, 140.00000, 141.00000, 142.00000, 143.00000, 144.00000, 145.00000, 146.00000, 147.00000, 148.00000, 149.00000, 150.00000, 151.00000, 152.00000, 153.00000, 0.00000, 0.00000, 0.00000, 0.00000, 2.00000, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 2.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 0.00000, 1.00000, 0.00000, 0.00000, 1.00000, 1.00000, 0.00000, 1.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 33.33333, 16.66667, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 33.33333, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 0.00000, 16.66667, 0.00000, 0.00000, 16.66667, 16.66667, 0.00000, 16.66667, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 16.66667, 0.00000, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 16.66667, 0.00000, 0.00000, 0.00000, 16.66667, 16.66667, 0.00000, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.66667, 0.00000, 0.00000, 0.00000 |

Other plotting functions
========================

These dataframes from the tidying functions are then used in these plots

gg\_missing\_var
----------------

``` r

gg_missing_var(airquality)
```

![](README-unnamed-chunk-9-1.png)

gg\_missing\_case
-----------------

``` r

gg_missing_case(airquality)
```

![](README-unnamed-chunk-10-1.png)

gg\_missing\_which
------------------

This shows whether

``` r

gg_missing_which(airquality)
```

![](README-unnamed-chunk-11-1.png)

heatmap of missing data?
------------------------

I recommend the use of the `vis_miss` function from the [`visdat`](github.com/njtierney/visdat) package.

``` r

# devtools::install_github("njtierney/visdat")
library(visdat)
vis_miss(airquality)
```

![](README-unnamed-chunk-12-1.png)

Future Work
===========

`ggmissing` will be undergoing more changes over the next 6 months, with plans to have the package in CRAN before the end of 2016.

Plans include: extend the `geom_missing` family to include:

-   1D, univariate distribution plots (e.g. as `geom_missing_histogram`)
-   Categorical variables
-   Bivariate plots:
    -   Scatterplots (`geom_missing_point`)
    -   Density overlays.
