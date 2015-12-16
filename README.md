# ggmissing

Currently, ggplot does not display missing data, omitting missing data from plots, but giving a warning message.

This repository is the beginnings of some R code to enable ggplot to display missingness.

GGobi and Manet provide methods of incorporating missingness. One approach is to replace "NA" values with values 10% lower than the minimum of that variable.

This is done with the `shadow_shift` function. This can be directly incorporated into ggplot with the following code.

But first, let's create some data using the wakefield package

```
library(wakefield)
# 
library(ggmissing)
# devtools::install_github("tierneyn/ggmissing")
library(ggplot2)
library(dplyr)

df <- 
  r_data_frame(
  n = 30,
  id,
  race,
  age,
  sex,
  hour,
  iq,
  height,
  died,
  Scoring = rnorm,
  Smoker = valid
  ) %>%
  r_na(prob=.4)


ggplot(data = df,
       aes(x = shadow_shift(Height),
           y = shadow_shift(Age))) +
  geom_point()
```

This allows missingness to be visualised, however the missing values would ideally be shown in a different colour, so that missingness becomes preattentive.

To do this, you can run the code below:

```
df %>%
  mutate(miss_cat = miss_cat(., "Height", "Age")) %>% 
  ggplot(data = .,
       aes(x = shadow_shift(Height),
           y = shadow_shift(Age),
           colour = miss_cat)) + 
  geom_point() 
```

Examples of the current work, in a stream-of-consciousness style can be seen in the vignette. The functions in this package are basically utility functions, `shadow_shift`, which shifts missing values to 10% below minimum, `shadow_df`  creates a shadow matrix, `miss_cat` creates a new column of missingness status that allows for the plot to create a factor out of missingness. `miss_cat` uses the utility function `shadow_cat`.

Future work will involve creating an elegant and meaningful way of coding and representing missingness into the data, ideally this would involve adding a `missing = T` option into geom_point(), something like this:

```
ggplot(data = df,
       aes(x = Height,
           y = Age)) + 
  geom_point(missing = T) 

```

One possible way forward would be to create a function inside `geom_point()`, called `missing`, that is set to FALSE on default, but when TRUE, it munches the appropriate data and produces the plot.

However, we will also need to consider other what other kinds of plots could be handled by this approach:

- 1D, univaritae distribution plots
- Categorical variables
- Bivariate plots: Scatterplots, Density overlays.
