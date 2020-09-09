#' Radiance Landsat OLI
#'
#' This function Convert Digital Numbers to TOA Radiance using the radiance rescaling factors in the MTL file.
#'
#' @param x A raster stack containing the first seven Landsat OLI bands.
#' @return A raster layer object with the Top of atmosphere spectral radiance.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' x <- stack(list.files(path_files,".tif", full.names=TRUE))
#' rad_oli(x)
#'
#' @references
#' \url{https://www.usgs.gov/land-resources/nli/landsat/using-usgs-landsat-level-1-data-product}.
#' @export
#' @import raster
rad_oli <- function (x) {
  B01 <-  ((0.012325*x[[1]]) + (-61.62500))
  B02 <-  ((0.012621*x[[2]]) + (-63.10476))
  B03 <-  ((0.01163*x[[3]]) + (-58.15049))
  B04 <-  ((0.0098072*x[[4]]) + (-49.03577))
  B05 <-  ((0.0060015*x[[5]]) + (-30.00745))
  B06 <-  ((0.0014925*x[[6]]) + (-7.46258))
  B07 <-  ((0.00050306*x[[7]]) + (-2.51529))
  stack(B01,B02,B03,B04,B05,B06,B07)
}
