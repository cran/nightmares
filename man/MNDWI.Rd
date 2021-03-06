% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MNDWI.R
\name{MNDWI}
\alias{MNDWI}
\title{MNDWI - Normalized Difference Water Index}
\usage{
MNDWI(G, SWIR1)
}
\arguments{
\item{G}{A raster layer object with the reflectance values for the Green band.}

\item{SWIR1}{A raster layer object with the reflectance values for the Short Wave Infrared band.}
}
\value{
MNDWI - Normalized Difference Water Index
}
\description{
MNDWI can enhance open water features while efficiently suppressing and even removing built-up land noise as well as vegetation and soil noise.
}
\examples{
library(raster)
path_files <- system.file("extdata/", package="nightmares")
bands <- stack(list.files(path_files,".tif", full.names=TRUE))
x <- ref_oli(bands, sun.elev= 67.97)
MNDWI(x[[3]], x[[6]])

}
\references{
Xu, H. 2006. Modification of normalised difference water index (NDWI) to enhance open water features in remotely sensed imagery. International Journal of Remote Sensing, 27(14), 3025-3033.
}
