#' Tasselled Cap Transformation
#'
#' This function Convert Reclectance values to Brightness, Greenness and Wetness.
#'
#' @param x A raster stack containing the reflectance values of the first seven Landsat OLI bands.
#' @return Layer 1 - Brightness.
#' @return Layer 2 - Greenness.
#' @return Layer 3 - Wetness.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' x <- stack(list.files(path_files,".tif", full.names=TRUE))
#' tasscap_oli(x)
#'
#' @references
#' Baig et al., 2014. Derivation of a tasselled cap transformation based on Landsat 8 at-satellite reflectance. Remote Sensing Letters 5(5), 423-431.
#' @export
#' @import raster
tasscap_oli <- function (x) {
  Brightness <- ((0.3029*x[[2]]) + (0.2786*x[[3]]) + (0.4733*x[[4]]) + (0.5599*x[[5]]) + (0.508*x[[6]]) + (0.1872*x[[7]]))
  Greenness <- ((-0.2941*x[[2]]) + (-0.243*x[[3]]) + (-0.5424*x[[4]]) + (0.7276*x[[5]]) + (0.0713*x[[6]]) + (-0.1608*x[[7]]))
  Wetness <- ((0.1511*x[[2]]) + (0.1973*x[[3]]) + (0.3283*x[[4]]) + (0.3407*x[[5]]) + (-0.7117*x[[6]]) + (-0.4559*x[[7]]))
  stack(Brightness, Greenness, Wetness)

}
