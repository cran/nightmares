#' NDMI - Normalized Difference Moisture Index
#'
#' NDMI is used to determine vegetation water content.
#'
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @param SWIR A raster layer object with the reflectance values for the Short Wave Infrared band.
#' @return NDMI - Normalized Difference Moisture Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' NDMI(x[[5]], x[[6]])
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
NDMI <- function (NIR, SWIR) {
  if (missing(NIR)) {
    stop("Required data missing. Please, select the reflectance values for the Near Infrared band")
  }
  if (missing(SWIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Short Wave Infrared band")
  }

  NDMI <- (NIR-SWIR)/(NIR+SWIR)

}
