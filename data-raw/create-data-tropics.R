# modify the tao data

library(narnia)
library(dplyr)

tropics <- tao %>%
  rename(sea_temp_c = sea.surface.temp,
         air_temp_c = air.temp,
         wind_ew = uwind,
         wind_ns = vwind) %>%
  as_tibble()

tropics

devtools::use_data(tropics, overwrite = TRUE)
# then delete the tao dataset
