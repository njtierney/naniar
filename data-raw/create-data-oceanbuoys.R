# modify the tao data from the MissingDataGUI dataset

library(naniar)
library(dplyr)

oceanbuoys <- tao %>%
  rename(sea_temp_c = sea.surface.temp,
         air_temp_c = air.temp,
         wind_ew = uwind,
         wind_ns = vwind) %>%
  as_tibble()

devtools::use_data(oceanbuoys, overwrite = TRUE)
# then delete the tao dataset
