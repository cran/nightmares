#' MSAVI - Modified Soil Adjusted Vegetation Index
#'
#' MSAVI minimizes the effect of bare soil on the Soil Adjusted Vegetation Index.
#'
#' @param R A raster layer object with the reflectance values for the Red band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @return MSAVI - Modified Soil Adjusted Vegetation Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' MSAVI(x[[4]], x[[5]])
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
MSAVI <- function (R, NIR) {
  if (missing(R)) {
    stop("Required data missing. Please, select the reflectance values for the Red band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }

  MSAVI <- (2 * NIR + 1 - sqrt((2 * NIR + 1)^2 - 8 * (NIR-R))) / 2

}
