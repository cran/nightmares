#' Atmospheric pressure
#'
#' This function is used to estimate the The atmospheric pressure, P, by using a simplification of the ideal gas law, assuming 20°C for a standard atmosphere.
#' Please, see the equation 7 from the FAO Irrigation and drainage paper 56.
#'
#' @param z Elevation above sea level in meters.
#' @return Atmospheric pressure in kPa.
#'
#' @examples
#' atm_pressure(z= 1120)
#'
#' @references
#' Allen et al., 1998. Crop Evapotranspiration. Guidelines for computing crop water requirements. FAO Irrigation and drainage paper 56.
#' \url{http://www.fao.org/3/X0490E/x0490e00.htm}.
#' @export
atm_pressure <- function(z) {
  if (missing(z)) {
    stop("Required data is missing. Please, specify the elevation above sea level in meters")
  }
  pressure <- round(as.numeric(101.3*(((293-(0.0065*z))/293)^5.26)),2)

}
