#' LAI - Leaf Area Index
#'
#' This function calculate the Leaf Area Index (LAI) based on Boegh et al., 2002. High LAI values typically range from approximately 0 to 3.5. However, when the scene contains clouds and other bright features that produce saturated pixels, the LAI values can exceed 3.5.
#'
#' @param EVI A raster layer object with the Enhanced Vegetation Index.
#' @return LAI - Leaf Area Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' evi <- EVI(x[[2]], x[[4]], x[[5]])
#' LAI(evi)
#'
#' @references
#' Boegh et al., 2002. Airborne Multi-spectral Data for Quantifying Leaf Area Index, Nitrogen Concentration and Photosynthetic Efficiency in Agriculture. Remote Sensing of Environment, 81(2-3), 179-193.
#' @export
#' @import raster
LAI <- function (EVI) {
  if (missing(EVI)) {
    stop("Required data missing. Please, enter the Enhanced Vegetation Index")
  }

  LAI <- (3.618*(EVI))-0.118

}
