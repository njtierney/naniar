# modify the brfss data from the MissingDataGUI dataset

library(tidyverse)
library(janitor)
library(naniar)

# downloaded from https://github.com/chxy/MissingDataGUI/blob/master/data/brfss.rda
# as I could not install MissingDataGUI
# load("~/Downloads/brfss.rda")

riskfactors <- brfss %>%
  tibble::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::rename(hispanic = hispanc2,
                veteran = veteran2,
                education = educa,
                employment = employ,
                income = income2,
                weight_lbs = weight2,
                height_inch = height3,
                health_general = genhlth,
                health_physical = physhlth,
                health_mental = menthlth,
                health_poor = poorhlth,
                health_cover = hlthplan,
                provide_care = caregive,
                activity_limited = qlactlm2,
                drink_any = drnkany4,
                drink_days = alcday4,
                drink_average = avedrnk2,
                smoke_100 = smoke100,
                smoke_days = smokday2,
                smoke_stop = stopsmk2,
                smoke_last = lastsmk1,
                diet_fruit = fruit,
                diet_salad = greensal,
                diet_potato = potatoes,
                diet_carrot = carrots,
                diet_vegetable = vegetabl,
                diet_juice = fruitjui,
                bmi = bmi4) %>%
  # change the order slightly to group together health/demographics
  dplyr::select(state,
                sex,
                age,
                weight_lbs,
                height_inch,
                bmi,
                marital,
                pregnant,
                children,
                education,
                employment,
                income,
                veteran,
                hispanic,
                dplyr::everything()) %>%
  # recode BMI to be decimal, as it should be
  dplyr::mutate(bmi = bmi / 100)

devtools::use_data(riskfactors, overwrite = TRUE)
# then delete the brfss dataset
