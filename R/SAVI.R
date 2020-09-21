#' SAVI - Soil Adjusted Vegetation Index
#'
#' SAVI is used to correct the Normalized Difference Vegetation Index for the influence of soil brightness in areas where vegetative cover is low.
#'
#' @param R A raster layer object with the reflectance values for the Red band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @return SAVI - Soil Adjusted Vegetation Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' SAVI(x[[4]], x[[5]])
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
SAVI <- function (R, NIR) {
  if (missing(R)) {
    stop("Required data missing. Please, select the reflectance values for the Red band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }

  SAVI <- ((NIR-R)/(NIR+R+0.5))*(1.5)

}
