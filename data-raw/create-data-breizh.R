# download the csv

library(tidyverse)
breizh <- read_csv2("https://breizh-sba.opendatasoft.com/explore/dataset/marches-publics-conseil-regional-de-bretagne/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")
breizh_nd <- sample_frac(breizh, size = 0.5)
breizh_vide <- anti_join(breizh, breizh_nd)
breizh_nd <- map_df(breizh_nd, function(x){gsub("Non disponible", "not_available",x)})
breizh_vide <- map_df(breizh_vide, function(x){gsub("Non disponible", "empty",x)})
breizh <- rbind(breizh_nd, breizh_vide)

devtools::use_data(breizh, overwrite = TRUE)
roxygen2::roxygenise()
