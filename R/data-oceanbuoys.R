#' West Pacific Tropical Atmosphere Ocean Data, 1993 & 1997.
#'
#' Real-time data from moored ocean buoys for improved detection, understanding
#'   and prediction of El Ni'o and La Ni'a. The data is collected by the
#'   Tropical Atmosphere Ocean project
#'   (\url{https://www.pmel.noaa.gov/tao/index.shtml}).
#'
#' Format: a data frame with 736 observations on the following 8
#' variables.
#' \describe{
#'   \item{`year`}{A numeric with levels `1993` `1997`.}
#'   \item{`latitude`}{A numeric with levels `-5`  `-2` `0`.}
#'   \item{`longitude`}{A numeric with levels `-110` `-95`.}
#'   \item{`sea_temp_c`}{Sea surface temperature(degree Celsius),  measured
#'    by the TAO buoys at one meter below the surface.}
#'   \item{`air_temp_c`}{Air temperature(degree Celsius), measured by the
#'     TAO buoys three meters above the sea surface.}
#'   \item{`humidity`}{Relative humidity(%), measured by the TAO buoys 3
#'     meters above the sea surface.}
#'   \item{`wind_ew`}{The East-West wind vector components(M/s).  TAO buoys
#'     measure the wind speed and direction four meters above the sea surface.
#'     If it is positive, the East-West component of the wind is blowing towards
#'     the East. If it is negative, this component is blowing towards the West.}
#'   \item{`wind_ns`}{The North-South wind vector components(M/s). TAO
#'     buoys measure the wind speed and direction four meters above the sea
#'     surface. If it is positive, the North-South component of the wind is
#'     blowing towards the North. If it is negative, this component is blowing
#'     towards the South.}}
#' @name oceanbuoys
#' @docType data
#' @usage data(oceanbuoys)
#' @source \url{https://www.pmel.noaa.gov/tao/data_deliv/deliv.html}
#' @keywords datasets
#' @seealso library(MissingDataGUI) (data named "tao")
#' @examples
#'
#' vis_miss(oceanbuoys)
#'
#' # Look at the missingness in the variables
#' miss_var_summary(oceanbuoys)
#' \dontrun{
#' # Look at the missingness in air temperature and humidity
#' library(ggplot2)
#' p <-
#' ggplot(oceanbuoys,
#'        aes(x = air_temp_c,
#'            y = humidity)) +
#'      geom_miss_point()
#'
#'  p
#'
#'  # for each year?
#'  p + facet_wrap(~year)
#'
#'  # this shows that there are more missing values in humidity in 1993, and
#'  # more air temperature missing values in 1997
#'
#'  # see more examples in the vignette, "getting started with naniar".
#' }
"oceanbuoys"
