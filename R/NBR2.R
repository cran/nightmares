#' NBR2 - Normalized Burn Ratio 2
#'
#' NBR2 modifies the Normalized Burn Ratio to highlight water sensitivity in vegetation and may be useful in post-fire recovery studies.
#'
#' @param SWIR1 A raster layer object with the reflectance values for the Short Wave Infrared 1 band.
#' @param SWIR2 A raster layer object with the reflectance values for the Short Wave Infrared 2 band.
#' @return NBR2 - Normalized Burn Ratio 2.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' NBR2(x[[6]], x[[7]])
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
NBR2 <- function (SWIR1, SWIR2) {
  if (missing(SWIR1)) {
    stop("Required data missing. Please, select the reflectance values for the Short Wave Infrared 1 band")
  }
  if (missing(SWIR2)) {
    stop("Required data missing. Please, enter the reflectance values for the Short Wave Infrared 2 band")
  }

  NBR2 <- (SWIR1-SWIR2)/(SWIR1+SWIR2)

}
