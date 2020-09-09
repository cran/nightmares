#' Reflectance Landsat OLI
#'
#' This function convert Digital Numbers to TOA Reflectance using the rescaling coefficients in the MTL file.
#'
#' @param x A raster stack containing the first seven Landsat OLI bands.
#' @param sun.elev Sun elevation angle in degrees.
#' @return A raster layer object with the Top of atmosphere planetary reflectance.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' x <- stack(list.files(path_files,".tif", full.names=TRUE))
#' ref_oli(x, 67.97)
#' ref_oli(x, sun.elev=67.97)
#'
#' @references
#' \url{https://www.usgs.gov/land-resources/nli/landsat/using-usgs-landsat-level-1-data-product}.
#' @export
#' @import raster
ref_oli <- function (x, sun.elev) {
  if (missing(sun.elev)) {
    stop("Required data missing for SUN ELEVATION")
  }

  images <- list()
  for (i in 1:7)
    images[[i]] <- (0.00002 * x[[i]] - 0.1)/(sin(sun.elev*0.017453278))
  stack(images)
}
