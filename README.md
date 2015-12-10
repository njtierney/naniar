# ggmissing

Currently, ggplot does not display missing data, omitting missing data from plots, but giving a warning message.

This repository is the beginnings of some R code to enable ggplot to display missingness.

GGobi and Manet provide methods of incorporating missingness. One approach is to replace "NA" values with values 10% lower than the minimum of that variable.

This is done with the `shadow_shift` function. This can be directly incorporated into ggplot:

```
ggplot(data = df,
       aes(x = shadow_shift(Height),
           y = shadow_shift(Age))) +
  geom_point()
```

This allows missingness to be visualised, however the missing values would ideally be shown in a different colour, so that missingness becomes preattentive. Examples of current work can be seen in the vignette.

We currently have a messy approach to colouring these points differently, although a more elegant solution is needed.

In this repository is a "vignette" of sorts, describing the process of adding missingness. Utility functions for plotting the missingness, `shadow_shift`, which shifts missing values, `shadow_df` which creates a shadow matrix, `miss_cat` which creates a new column of missingness status that allows for the plot to create a factor out of missingness. `miss_cat` uses the utility function `shadow_cat`.

Future work will involve creating an elegant and meaningful way of coding and representing missingness into the data, ideally this would involve adding a `missing = T` option into geom_point(). However, we will also need to consider other what other kinds of plots could be handled by this approach:

- 1D, univaritae distribution plots
- Categorical variables
- Bivariate plots: Scatterplots, Density overlays,
