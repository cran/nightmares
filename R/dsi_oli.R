#' Landsat Surface Reflectance Derived Spectral Indices
#'
#' This function requires Landsat Surface Reflectance from Landsat 8 Operational Land Imager to derive spectral indices.
#'
#' @param x A raster layer object with the Top of atmosphere planetary reflectance with at least the first seven bands.
#' @return Layer 1 - Normalized Difference Vegetation Index
#' @return Layer 2 - Enhanced Vegetation Index
#' @return Layer 3 - Soil Adjusted Vegetation Index
#' @return Layer 4 - Modified Soil Adjusted Vegetation Index
#' @return Layer 5 - Normalized Difference Moisture Index
#' @return Layer 6 - Normalized Burn Ratio
#' @return Layer 7 - Normalized Burn Ratio 2
#' @return Layer 8 - Advanced Vegetation Index
#' @return Layer 9 - Bare Soil Index
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' bands <- stack(list.files(path_files,".tif", full.names=TRUE))
#' x <- ref_oli(bands, sun.elev= 67.97)
#' dsi_oli(x)
#'
#' @references
#' \url{https://www.usgs.gov/core-science-systems/nli/landsat/landsat-surface-reflectance-derived-spectral-indices}.
#' \url{https://www.geo.university/pages/spectral-indices-with-multispectral-satellite-data}.
#' @export
#' @import raster
dsi_oli <- function (x) {
  NDVI <- (x[[5]]-x[[4]])/(x[[5]]+x[[4]])
  EVI <- 2.5 * ((x[[5]] - x[[4]]) / (x[[5]] + 6*x[[4]] - 7.5*x[[2]] + 1))
  SAVI <- ((x[[5]] - x[[4]]) / (x[[5]] + x[[4]] + 0.5)) * (1.5)
  MSAVI <- (2 * x[[5]] + 1 - sqrt((2 * x[[5]] + 1)^2 - 8 * (x[[5]] - x[[4]]))) / 2
  NDMI <- (x[[5]] - x[[6]]) / (x[[5]] + x[[6]])
  NBR <- (x[[5]] - x[[7]]) / (x[[5]] + x[[7]])
  NBR2 <- (x[[6]] - x[[7]]) / (x[[6]] + x[[7]])
  AVI <- (x[[5]]*(1-x[[4]])*(x[[5]]-x[[4]]))^(1/3)
  BSI <- ((x[[6]] + x[[4]])-(x[[5]] + x[[2]])) / ((x[[6]] + x[[4]])+(x[[5]] + x[[2]]))

  stack(NDVI, EVI, SAVI, MSAVI, NDMI, NBR, NBR2, AVI, BSI)
}
