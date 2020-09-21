#' GCI - Green Chlorophyll Index
#'
#' This function is used to estimate leaf chlorophyll content of plants species.
#'
#' @param G A raster layer object with the reflectance values for the Green band.
#' @param NIR A raster layer object with the reflectance values for the Near Infrared band.
#' @return GCI - Green Chlorophyll Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' GCI(x[[3]], x[[5]])
#'
#' @references
#' Gitelson et al., 2003. Relationships between leaf chlorophyll content and spectral reflectance and algorithms for non-destructive chlorophyll assessment in higher plant leaves. Journal of Plant Physiology, 160, 271-282.
#' @export
#' @import raster
GCI <- function (G, NIR) {
  if (missing(G)) {
    stop("Required data missing. Please, enter the reflectance values for the Green band")
  }
  if (missing(NIR)) {
    stop("Required data missing. Please, enter the reflectance values for the Near Infrared band")
  }

  GCI <- ((NIR/G)-1)

}
