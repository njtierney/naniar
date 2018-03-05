# Work in prgoress for geom_miss_line

# OK, so the idea is that we want to receate this:

library(imputeTS)
plotNA.distribution(tsAirgap)

# what are the steps to do this?

# first identify the gaps?

library(tidyverse)

naniar::miss_var_run(pedestrian, hourly_counts)

library(tsibble)

tsAirgap %>%
  tsibble(key = id(month),
          index = year)

ggplot(aes(x = date_time,
           y = hourly_counts)) +
  geom_line()


