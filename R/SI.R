#' SI - Shadow Index
#'
#' SI has main applications in forestry and crop monitoring. The characteristics of canopy shadow are associated by the total spectral radiance that is reflected from the canopy. Canopy shadow provides essential information about trees and plants arrangement.
#'
#' @param B A raster layer object with the reflectance values for the Blue band.
#' @param G A raster layer object with the reflectance values for the Green band.
#' @param R A raster layer object with the reflectance values for the Red band.
#' @return SI - Shadow Index
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' SI(x[[2]], x[[3]], x[[4]])
#'
#' @references
#' Rikimaru et al., 2002. Tropical forest cover density mapping. Tropical Ecology, 43, 39-47.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
SI <- function (B, G, R) {
  if (missing(B)) {
    stop("Required data missing. Please, enter the reflectance values for the Blue band")
  }
  if (missing(G)) {
    stop("Required data missing. Please, enter the reflectance values for the Green band")
  }
  if (missing(R)) {
    stop("Required data missing. Please, select the reflectance values for the Red band")
  }

  SI <- ((1-B)*(1-G)*(1-R))^(1/3)

}
