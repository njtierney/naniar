#' Pedestrian count information around Melbourne for 2015 and 2016
#'
#' This dataset contains hourly counts of pedestrians from 43 sensors around Melbourne,
#'   recorded from January 1st 2015 at 00:00:00 to December 31st 2016 at 23:00:00. The
#'   data is made free and publicly available from \url{https://data.melbourne.vic.gov.au/Transport-Movement/Pedestrian-volume-updated-monthly-/b2ak-trbp}
#'
#' @format A tibble with 761,168 rows and 9 variables:
#' \describe{
#'   \item{hourly_counts}{(integer) the number of pedestrians counted at that sensor at
#'     that time}
#'   \item{date_time}{(POSIXct, POSIXt) The time that the count was taken}
#'   \item{year}{(double) Year of record}
#'   \item{month}{(factor) Month of record as an ordered factor (1 = January, 12 =
#'     December)}
#'   \item{month_day}{(integer) Full day of the month}
#'   \item{week_day}{(factor) Full day of the week as an ordered factor (1 = Sunday, 7 =
#'     Saturday)}
#'   \item{hour}{(integer) The hour of the day in 24 hour format}
#'   \item{sensor_id}{(integer) the id of the sensor}
#'   \item{sensor_name}{(character) the full name of the sensor}
#' }
#'
"pedestrian"
