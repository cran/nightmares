#' Determination of ETo with missing data
#'
#' This function is used to the estimation of monthly ETo with the FAO Penman-Montheith equation for a data set containing only maximum and minimum air temperature.
#' Please, see the example 20 from the FAO Irrigation and drainage paper 56.
#'
#' @param lat Latitude in decimal degrees.
#' @param z Elevation above sea level in meters.
#' @param doy Day of the year.
#' @param tmax Maximum air temperature in Celsius degrees.
#' @param tmin Minimum air temperature in Celsius degrees.
#' @param tdew Dewpoint temperature in Celsius degrees.
#' @param u2 Wind speed at 2 m height in m s-1.
#' @param Rs Solar radiation in MJ/sq.m/day.
#' @param P Atmospheric pressure  in kPa.
#' @return Reference Evapotranspiration in mm day-1
#'
#' @examples
#' ETo56_miss(45.72, 200, 196, 26.6, 14.8, 14.8, 2, 22.29, 98.96)
#'
#' @references
#' Allen et al., 1998. Crop Evapotranspiration. Guidelines for computing crop water requirements. FAO Irrigation and drainage paper 56.
#' \url{http://www.fao.org/3/X0490E/x0490e00.htm}.
#' @export
ETo56_miss <- function(lat, z, doy, tmax, tmin, tdew, u2, Rs, P) {
  if (missing(lat)) {
    stop("Required data is missing. Please, specify the latitude in decimal degrees")
  }
  if (missing(z)) {
    stop("Required data is missing. Please, specify the altitude in meters above sea level")
  }
  if (missing(doy)) {
    stop("Required data is missing. Please, specify the day of the year")
  }
  if (missing(tmax)) {
    stop("Required data is missing. Please, specify the maximum temperature in Celsius degrees")
  }
  if (missing(tmin)) {
    stop("Required data is missing. Please, specify the minimum temperature in Celsius degrees")
  }
  if (missing(tdew)) {
    stop("Required data is missing. Please, specify the dewpoint temperature in Celsius degrees. If unknown, please specify the same value as tmin")
  }
  if (missing(u2)) {
    stop("Required data is missing. Please, specify the wind speedin m/s. If unknown, please specify a value of 2")
  }
  if (missing(Rs)) {
    stop("Required data is missing. Please, specify the Solar radiation in MJ/sq.m/d. If unknown, please use the solrad function to calculate it.")
  }
  if (missing(P)) {
    stop("Required data is missing. Please, specify the atmospheric pressure in kPa. If unknown, please use the atm_pressure function to calculate it.")
  }
  lat_rad <- as.numeric((pi/180)*lat)

  tmean <- as.numeric(0.5*(tmax+tmin))

  gama <- as.numeric(0.000665*P)

  delta <- as.numeric((4099*(0.6108*exp((17.27*tmean)/(tmean+237.3))))/((tmean+237.3)^2))
  ea <- as.numeric(0.6108*exp((17.27*tdew)/(tdew+237.3)))
  es <- as.numeric(((0.6108*exp((17.27*tmax)/(tmax+237.3)))+(0.6108*exp((17.27*tmin)/(tmin+237.3))))*0.5)

  Gsc <- as.numeric(0.082)
  dr <- as.numeric(1+(0.033*cos(((2*pi)/365)*doy)))
  s_de <- as.numeric(0.409*sin((2*pi/365)*doy-1.39))
  ws <- as.numeric(acos(-tan(lat_rad)*tan(s_de)))

  Ra <- as.numeric((24*60/pi)*Gsc*dr*(ws*sin(lat_rad)*sin(s_de)+cos(lat_rad)*cos(s_de)*sin(ws)))

  Rso <- as.numeric((0.75+((2*z)/100000))*Ra)
  Rns <- as.numeric((1-0.23)*Rs)
  sigma <- as.numeric(0.000000004903)
  Rnl <- as.numeric((sigma*((((tmax+273.16)**4)+((tmin+273.16)**4))/2))*(0.34-0.14*sqrt(ea))*(1.35*(Rs/Rso)-0.35))
  Rn <- as.numeric(Rns-Rnl)
  ET0 <- round(as.numeric(((0.408*delta*Rn)+(gama*(900/(tmean+273)))*(u2*(es-ea)))/(delta + gama*(1+(0.34*u2)))),2)

}
