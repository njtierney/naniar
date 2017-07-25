# modify the tao data

library(naniar)
library(dplyr)

oceanbuoys <- tao %>%
  rename(sea_temp_c = sea.surface.temp,
         air_temp_c = air.temp,
         wind_ew = uwind,
         wind_ns = vwind) %>%
  as_tibble()

# oceanbuoys <- tropics

devtools::use_data(oceanbuoys, overwrite = TRUE)
# then delete the tao dataset
