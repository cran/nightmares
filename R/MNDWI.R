#' MNDWI - Normalized Difference Water Index
#'
#' MNDWI can enhance open water features while efficiently suppressing and even removing built-up land noise as well as vegetation and soil noise.
#'
#' @param G A raster layer object with the reflectance values for the Green band.
#' @param SWIR1 A raster layer object with the reflectance values for the Short Wave Infrared band.
#' @return MNDWI - Normalized Difference Water Index
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' MNDWI(x[[3]], x[[6]])
#'
#' @references
#' Xu, H. 2006. Modification of normalised difference water index (NDWI) to enhance open water features in remotely sensed imagery. International Journal of Remote Sensing, 27(14), 3025-3033.
#' @export
#' @import raster
MNDWI <- function (G, SWIR1) {
  if (missing(G)) {
    stop("Required data missing. Please, select the reflectance values for the Green band")
  }
  if (missing(SWIR1)) {
    stop("Required data missing. Please, enter the reflectance values for the Short Wave Infrared band")
  }

  MNDWI <- (G-SWIR1)/(G+SWIR1)

}
