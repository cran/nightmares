#' AVI - Advanced Vegetation Index
#'
#' AVI is similar to NDVI and it is used in vegetation studies to monitor crop and forest variations over time. Through the multi-temporal combination of the AVI and the NDVI, users can discriminate different types of vegetation and extract phenology characteristics.
#'
#' @param R A raster layer object with the reflectance values for the Red band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @return AVI - Advanced Vegetation Index
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' AVI(x[[4]], x[[5]])
#'
#' @references
#' Rikimaru et al., 2002. Tropical forest cover density mapping. Tropical Ecology, 43, 39-47.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
AVI <- function (R, NIR) {
  if (missing(R)) {
    stop("Required data missing. Please, select the reflectance values for the Red band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }

  AVI <- (NIR*(1-R)*(NIR-R))^(1/3)

}
