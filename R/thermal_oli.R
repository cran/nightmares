#' Top of Atmosphere Brightness Temperature
#'
#' This function Convert Digital Numbers to Top of Atmosphere Brightness Temperature.
#'
#' @param x A raster containing the Landsat OLI band 10 or 11.
#' @param band Specify if your raster is the Landsat OLI band 10 or 11
#' @param units specify if your desired units are Celsius, Kelvin, or Fahrenheit degrees
#' @return A raster layer object with the Top of atmosphere brightness temperature.
#'
#' @examples
#' library(raster)
#' B10 <- raster(system.file("extdata/3047_20190517_B10.tif", package="nightmares"))
#' B11 <- raster(system.file("extdata/3047_20190517_B11.tif", package="nightmares"))
#' thermal_oli(B10, 10, "Celsius")
#' thermal_oli(B11, 11, "Fahrenheit")
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/using-usgs-landsat-level-1-data-product}.
#' @export
#' @import raster
thermal_oli <- function (x, band = c(10, 11), units = c(Celsius, Kelvin, Fahrenheit)) {
  if (missing(band)) {
    stop("Required data missing for BAND. Please, select the band 10 or 11")
  }
  if (missing(units)) {
    stop("Required data missing for UNITS. Please, select between Celsius, Kelvin or Fahrenheit")
  }

  TOA <- (0.0003342*x)+0.1

  if (band == 10 & units == "Celsius") {

    temperature <- (1321.0789/log((774.8853/TOA)+1)) - 273.15

  } else if (band == 10 & units == "Kelvin") {

    temperature <- 1321.0789/log((774.8853/TOA)+1)

  } else if (band == 10 & units == "Fahrenheit") {

    temperature <- (1321.0789/log((774.8853/TOA)+1))* (1.8) - 459.67

  } else if (band == 11 & units == "Celsius") {

    temperature <- (1201.1442/log((480.8883/TOA)+1)) - 273.15

  } else if (band == 11 & units == "Kelvin") {

    temperature <- 1201.1442/log((480.8883/TOA)+1)

  } else if (band == 11 & units == "Fahrenheit") {

    temperature <- (1201.1442/log((480.8883/TOA)+1))* (1.8) - 459.67

  }

}
