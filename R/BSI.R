#' BSI - Bare Soil Index
#'
#' BSI can be used for soil mapping and crop identification. This index relates the Blue, Red, Near Infrared and Short Wave Infrared bands.
#'
#' @param B A raster layer object with the reflectance values for the Blue band.
#' @param R A raster layer object with the reflectance values for the Red band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @param SWIR1 A raster layer object with the reflectance values for the Short Wave Infrared band.
#' @return BSI - Bare Soil Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' BSI(x[[2]], x[[4]], x[[5]], x[[6]])
#'
#' @references
#' Rikimaru et al., 2002. Tropical forest cover density mapping. Tropical Ecology, 43, 39-47.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
BSI <- function (B, R, NIR, SWIR1) {
  if (missing(B)) {
    stop("Required data missing. Please, enter the reflectance values for the Blue band")
  }
  if (missing(R)) {
    stop("Required data missing. Please, select the reflectance values for the Red band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }
  if (missing(SWIR1)) {
    stop("Required data missing. Please, enter the reflectance values for the Short Wave Infrared band")
  }

  BSI <- ((SWIR1+R)-(NIR+B))/((SWIR1+R)+(NIR+B))

}
