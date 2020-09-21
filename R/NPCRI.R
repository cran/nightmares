#' NPCRI - Normalized Pigment Chlorophyll Ratio Index
#'
#' NPCRI is an index that is associated with the chlorophyll content and can find applications in precission agriculture. Using the red and blue spectral bands, NPCRI captures the information needed to quantify chlorophyll and Nitrogen.
#'
#' @param B A raster layer object with the reflectance values for the Blue band.
#' @param R A raster layer object with the reflectance values for the Red band.
#' @return NPCRI - Normalized Pigment Chlorophyll Ratio Index.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' NPCRI(x[[2]], x[[4]])
#'
#' @references
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
NPCRI <- function (B, R) {
  if (missing(B)) {
    stop("Required data missing. Please, enter the reflectance values for the B band")
  }
  if (missing(R)) {
    stop("Required data missing. Please, enter the reflectance values for the Red band")
  }

  NPCRI <- ((R-B)/(R+B))

}
