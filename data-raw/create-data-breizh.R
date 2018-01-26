# Download the file

download.file(url = "https://breizh-sba.opendatasoft.com/explore/dataset/marches-publics-collectivites-bretonnes/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true", destfile = "breizh.csv")

breizh <- read.csv2("breizh.csv", stringsAsFactors = FALSE)

# Remove non ascii char so CRAN will be happy \o/
names(breizh) <- proustr::unacent(names(breizh))
breizh <- purrr::map_df(breizh, proustr::unacent)

# Save + compress
usethis::use_data(breizh, overwrite = TRUE, compress = "xz")

# delete breizh.csv
fs::file_delete("breizh.csv")
