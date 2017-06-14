# # using json...
# tmp <- tempfile()
# curl::curl_download(url = "https://data.melbourne.vic.gov.au/resource/mxb8-wn4w.json",
#                     destfile = tmp)
#
# melb_json <- jsonlite::fromJSON(tmp, simplifyDataFrame = TRUE) %>% tibble::as_tibble()
#
# melb_json

library(readr)
library(tidyverse)
library(magrittr)

# Data cleaning code largely written by Earo Wang.

# data taken from https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp

dat_ped <- read_csv("../earo-miss/Pedestrian_volume__updated_monthly_.csv") %>%
  filter(Year > 2013) %>%
  mutate(Date_Time = lubridate::dmy_hm(Date_Time))

# full_seq(c(1,3,5), period = 1)

# Make missing values explict
df_time <- dat_ped %>%
  split(.$Sensor_ID) %>%
  map(extract2, "Date_Time") %>%
  map(full_seq, period = 3600)

sensors_ls <- dat_ped %>%
  distinct(Sensor_ID, Sensor_Name) %>%
  arrange(Sensor_ID) %>%
  split(.$Sensor_ID)

df_time <- map2(df_time, sensors_ls, data.frame) %>%
  map(set_colnames, c("Date_Time", "Sensor_ID", "Sensor_Name"))

ped_full <- dat_ped %>%
  split(.$Sensor_ID) %>%
  map2(df_time, right_join,
       by = c("Date_Time", "Sensor_ID", "Sensor_Name")) %>%
  bind_rows()

# could possibly use tidyr::complete?
# dat_ped_com <- dat_ped %>%
#   complete(Date_Time = full_seq(Date_Time, 3600))

library(lubridate)
# ped_post_2015 <- ped_full %>%
#   filter(Date_Time > "2014-12-31 23:00:00 UTC",
#          Date_Time < "2017-01-01 01:00:00 UTC")

pedestrian <- ped_full %>%
  mutate(Year = as.integer(year(Date_Time)),   # force year to be an integer
         Month = month(Date_Time, label = TRUE, abbr = FALSE),
         Mdate = mday(Date_Time),
         Day = wday(Date_Time, label = TRUE, abbr = FALSE),
         Time = hour(Date_Time)) %>%
  # drop id, as it does not give us much extra information
  select(-ID) %>%
  rename(date_time = Date_Time,
         year = Year,
         month = Month,
         month_day = Mdate,
         week_day = Day,
         hour = Time,
         sensor_id = Sensor_ID,
         sensor_name = Sensor_Name,
         hourly_counts = Hourly_Counts) %>%
  filter(year == 2016) %>%
  # Birrarung Marr, Bourke Street Mall, Flagstaff, Spencer St-Collins St (south)
  filter(sensor_id %in% c(2,7,23,13)) %>%
  select(hourly_counts,
         everything())

use_data(pedestrian,
         compress = "xz",
         overwrite = TRUE)

# used tools::resaveRdaFiles("data/pedestrian.rda")
# then used tools::checkRdaFiles("data/pedestrian.rda")
# to find that `xz` was the best value to use
#
# pedestrian_ts <- as.ts(pedestrian)
#
# use_data(pedestrian_ts,
#          compress = "xz",
#          overwrite = TRUE)
#

# rename vars and case to cols and row
# library(dplyr)
# library(ggplot2)
#
# pedestrian %>%
#   # filter(sensor_id %in% c(1,2,13,22,34,38)) %>%
#   # filter(sensor_id %in% c(1,2,13)) %>%
#   group_by(sensor_name, sensor_id) %>%
#   summarise(n_miss = n_miss(hourly_counts),
#             prop_miss = prop_miss(hourly_counts),
#             n_complete = n_complete(hourly_counts)) %>%
#   arrange(-n_miss)
#
# pedestrian %>%
#   filter(sensor_id %in% c(2,7,23,13)) %>%
#   filter(year == 2016)
#   group_by(sensor_name, sensor_id, year) %>%
#   summarise(n_miss = n_miss(hourly_counts),
#             prop_miss = prop_miss(hourly_counts),
#             n_complete = n_complete(hourly_counts),
#             total = n())
#
# pedestrian %>%
#   # filter(sensor_id %in% c(7,2,23,2,13)) %>%
#   # filter(sensor_id %in% c(7,2,23,13)) %>%
#   # filter(year == 2015) %>%
#   filter(sensor_id %in% c(2,7,23,13)) %>%
#   filter(year == 2016) %>%
#   ggplot(aes(x = date_time,
#              y = hourly_counts)) +
#   geom_line() +
#   facet_wrap(~sensor_name)
#   summarise(mean = mean(hourly_counts, na.rm = TRUE),
#             sd = sd(hourly_counts, na.rm = TRUE))
