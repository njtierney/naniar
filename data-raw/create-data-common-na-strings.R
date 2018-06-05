common_na_strings <- c(
  "NA",
  "N A",
  "N/A",
  "NA ",
  " NA",
  "N /A",
  "N / A",
  " N / A",
  "N / A ",
  "na",
  "n a",
  "n/a",
  "na ",
  " na",
  "n /a",
  "n / a",
  " a / a",
  "n / a ",
  "NULL",
  "null",
  "",
  "?",
  "*",
  ".",
  "-"
)

devtools::use_data(common_na_strings, overwrite = TRUE)
