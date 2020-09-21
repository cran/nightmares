#' NDWI - Normalized Difference Water Index
#'
#' This index is designed to maximize reflectance of water by using green wavelengths; minimize the low reflectance of NIR by water features; and take advantage of the high reflectance of NIR by vegetation and soil features. Water features have positive values and thus are enhanced, while vegetation and soil usually have zero or negative values and therefore are suppressed.The NDWI is useful in crop health monitoring, land-water boarding mapping, inland water discrimination from open sea water bodies, among others.
#'
#' @param G A raster layer object with the reflectance values for the Green band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @return NDWI - Normalized Difference Water Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' NDWI(x[[3]], x[[5]])
#'
#' @references
#' McFeeters, SK. 1996. The use of the Normalized Difference Water Index (NDWI) in the delineation of open water features. International Journal of Remote Sensing, 17(7), 1425-1432.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
NDWI <- function (G, NIR) {
  if (missing(G)) {
    stop("Required data missing. Please, select the reflectance values for the Green band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }

  NDWI <- (G-NIR)/(G+NIR)

}
