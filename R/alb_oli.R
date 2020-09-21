#' Albedo
#'
#' This function is used to Convert Reflectance values to Albedo.
#'
#' @param x A raster stack containing the reflectance values of the first seven Landsat OLI bands.
#' @param method Specify the method to be used (Chemin, Liang, Olmedo, Silva or Tasumi)
#' @return A raster layer object with the Albedo values.
#'
#' @examples
#' library(raster)
#' path_files <- system.file("extdata/", package="nightmares")
#' x <- stack(list.files(path_files,".tif", full.names=TRUE))
#' alb_oli(x, method="Chemin")
#' alb_oli(x, method="Liang")
#' alb_oli(x, "Tasummi")
#'
#' @references
#' Chemin Method, please see i.albedo function (GRASS). Only for OLI images.
#'
#' Liang, S. 2000. Narrowband to broadband conversions of land surface albedo I: Algorithms. Remote Sensing of Environment, 76(2), 213-238.
#'
#' Olmedo et al., 2016. water: Tools and functions to estimate actual evapotranspiration Using land surface energy balance models in R. The R journal, 8(2), 352-369.
#'
#' Silva et al., 2016. Procedures for calculation of the albedo with OLI-Landsat 8 images: Application to the Brazilian semi-arid. Revista Brasileira de Engenharia Agrícola e Ambiental, 20(1), 3-8.
#'
#' Tasumi et al., 2008. At-Surface Reflectance and Albedo from Satellite for Operational Calculation of Land Surface Energy Balance. Journal of Hydrologic Engineering, 13, 51-63.
#' @export
#' @import raster
alb_oli <- function (x,  method = c(Chemin, Liang, Olmedo, Silva, Tasumi)) {
  if (missing(method)) {
    stop("Required data missing for METHOD. Please, select between Chemin, Liang, Olmedo, Silva or Tasumi")
  }

  if (method == "Chemin") {

    alb <- 0.058674 + (2.153642*x[[1]]) - (2.242688*x[[2]]) - (0.520669*x[[3]]) + (0.622670*x[[4]]) + (0.129979*x[[5]]) - (0.047970*x[[6]]) + (0.15228*x[[7]])

  } else if (method == "Liang") {

    alb <- ((0.356*x[[2]]) + (0.130*x[[4]]) + (0.373*x[[5]]) + (0.085*x[[6]]) + (0.072*x[[7]]) - 0.0018) / (0.356 + 0.130 + 0.373 + 0.085 + 0.072)

  } else if (method == "Olmedo") {

    alb <- ((0.246*x[[2]]) + (0.146*x[[3]]) + (0.191*x[[4]]) + (0.304*x[[5]]) + (0.105*x[[6]]) + (0.008*x[[7]]))

  } else if (method == "Silva") {

    alb <- ((0.300*x[[2]]) + (0.277*x[[3]]) + (0.233*x[[4]]) + (0.143*x[[5]]) + (0.036*x[[6]]) + (0.012*x[[7]]))

  } else if (method == "Tasumi") {

    alb <- ((0.254*x[[2]]) - (0.149*x[[3]]) + (0.147*x[[4]]) + (0.311*x[[5]]) - (0.103*x[[6]]) + (0.036*x[[7]]))

  }

}
