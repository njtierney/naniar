# modify the tao data from the MissingDataGUI dataset
# library(MissingDataGUI)
library(dplyr)

# downloaded from https://github.com/chxy/MissingDataGUI/blob/master/data/tao.rda
# as I could not install MissingDataGUI
# load("~/Downloads/tao.rda")

oceanbuoys <- tao %>%
  rename(sea_temp_c = sea.surface.temp,
                air_temp_c = air.temp,
         wind_ew = uwind,
         wind_ns = vwind) %>%
  as_tibble() %>%
  # convert the factors in a sane way
  mutate_if(is.factor, .funs = function(x) as.numeric(as.character(x)))

devtools::use_data(oceanbuoys, overwrite = TRUE)
# then delete the tao dataset
