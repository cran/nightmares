#' NBR - Normalized Burn Ratio
#'
#' NBR is used to identify burned areas and provide a measure of burn severity.
#'
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @param SWIR2 A raster layer object with the reflectance values for the Short Wave Infrared band.
#' @return NBR - Normalized Burn Ratio.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' NBR(x[[5]], x[[7]])
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
NBR <- function (NIR, SWIR2) {
  if (missing(NIR)) {
    stop("Required data missing. Please, select the reflectance values for the Near Infrared band")
  }
  if (missing(SWIR2)) {
    stop("Required data missing. Please, enter the reflectance values for the Short Wave Infrared 2 band")
  }

  NBR <- (NIR-SWIR2)/(NIR+SWIR2)

}
