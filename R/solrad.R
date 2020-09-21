#' Solar radiation from air temperature differences
#'
#' This function is used to estimate the Solar Radiation in MJ/sq.m/day from air temperature differences by using the Hargreaves radiation formula.
#' Please, see the equation 50 from the FAO Irrigation and drainage paper 56.
#'
#' @param kRs Adjustment coefficient. Interior locations = 0.16, ..., Coastal locations = 0.19.
#' @param tmax Maximum air temperature in Celsius degrees.
#' @param tmin Minimum air temperature in Celsius degrees.
#' @param lat Latitude in decimal degrees.
#' @param doy Day of the year.
#' @return Solar radiation in MJ/sq.m/day.
#'
#' @examples
#' solrad(kRs= 0.19, tmax= 25, tmin= 15, lat= 43.5, doy= 150)
#'
#' @references
#' Allen et al., 1998. Crop Evapotranspiration. Guidelines for computing crop water requirements. FAO Irrigation and drainage paper 56.
#' \url{http://www.fao.org/3/X0490E/x0490e00.htm}.
#' @export
solrad <- function(kRs, tmax, tmin, lat, doy) {
  if (missing(kRs)) {
    stop("Required data is missing. Please, specify the Adjustment coefficient. Interior locations = 0.16, ..., Coastal locations = 0.19")
  }
  if (missing(tmax)) {
    stop("Required data is missing. Please, specify the Maximum air temperature in Celsius degrees")
  }
  if (missing(tmin)) {
    stop("Required data is missing. Please, specify the Minimum air temperature  in Celsius degrees")
  }
  if (missing(lat)) {
    stop("Required data is missing. Please, specify the latitude in decimal degrees")
  }
  if (missing(doy)) {
    stop("Required data is missing. Please, specify the day of the year")
  }
  lat_rad <- as.numeric((pi/180)*lat)

  Gsc <- as.numeric(0.082)
  dr <- as.numeric(1+(0.033*cos(((2*pi)/365)*doy)))
  s_de <- as.numeric(0.409*sin((2*pi/365)*doy-1.39))
  ws <- as.numeric(acos(-tan(lat_rad)*tan(s_de)))

  Ra <- as.numeric((24*60/pi)*Gsc*dr*(ws*sin(lat_rad)*sin(s_de)+cos(lat_rad)*cos(s_de)*sin(ws)))

  Rs <- round(as.numeric(kRs*sqrt(tmax-tmin)*Ra),2)

}
