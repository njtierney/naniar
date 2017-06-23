#' West Pacific Tropical Atmosphere Ocean Data, 1993 & 1997.
#'
#' Real-time data from moored ocean buoys for improved detection,
#' understanding and prediction of El Ni'o and La Ni'a.
#'
#' The data is collected by the Tropical Atmosphere Ocean project (
#' \url{http://www.pmel.noaa.gov/tao/index.shtml}).
#'
#' Format: a data frame with 736 observations on the following 8
#' variables.
#' \describe{
#'   \item{\code{year}}{A factor with levels \code{1993} \code{1997}.}
#'   \item{\code{latitude}}{A factor with levels \code{-5}  \code{-2} \code{0}.}
#'   \item{\code{longitude}}{A factor with levels \code{-110} \code{-95}.}
#'   \item{\code{sea_temp_c}}{Sea surface temperature(degree Celsius),  measured
#'    by the TAO buoys at one meter below the surface.}
#'   \item{\code{air_temp_c}}{Air temperature(degree Celsius), measured by the
#'     TAO buoys three meters above the sea surface.}
#'   \item{\code{humidity}}{Relative humidity(%), measured by the TAO buoys 3
#'     meters above the sea surface.}
#'   \item{\code{wind_ew}}{The East-West wind vector components(M/s).  TAO buoys
#'     measure the wind speed and direction four meters above the sea surface.
#'     If it is positive, the East-West component of the wind is blowing towards
#'     the East. If it is negative, this component is blowing towards the West.}
#'   \item{\code{wind_ns}}{The North-South wind vector components(M/s). TAO
#'     buoys measure the wind speed and direction four meters above the sea
#'     surface. If it is positive, the North-South component of the wind is
#'     blowing towards the North. If it is negative, this component is blowing
#'     towards the South.}}
#' @name oceanbuoys
#' @docType data
#' @usage data(oceanbuoys)
#' @source \url{http://www.pmel.noaa.gov/tao/data_deliv/deliv.html}
#' @keywords datasets
#' @seealso library(MissingDataGUI) (data named "tao")
"oceanbuoys"
