#' EVI - Enhanced Vegetation Index
#'
#' EVI is similar to the Normalized Difference Vegetation Index. However, EVI corrects for some atmospheric conditions and canopy background noise and is more sensitive in areas with dense vegetation.
#'
#' @param B A raster layer object with the reflectance values for the Blue band.
#' @param R A raster layer object with the reflectance values for the Red band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @return EVI - Enhanced Vegetation Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' EVI(x[[2]], x[[4]], x[[5]])
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
EVI <- function (B, R, NIR) {
  if (missing(B)) {
    stop("Required data missing. Please, select the reflectance values for the Blue band")
  }
  if (missing(R)) {
    stop("Required data missing. Please, select the reflectance values for the Red band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }

  EVI <- 2.5 * (NIR-R)/(NIR+6*R-7.5*B+1)

}
