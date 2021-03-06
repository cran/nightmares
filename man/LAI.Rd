% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LAI.R
\name{LAI}
\alias{LAI}
\title{LAI - Leaf Area Index}
\usage{
LAI(EVI)
}
\arguments{
\item{EVI}{A raster layer object with the Enhanced Vegetation Index.}
}
\value{
LAI - Leaf Area Index.
}
\description{
This function calculate the Leaf Area Index (LAI) based on Boegh et al., 2002. High LAI values typically range from approximately 0 to 3.5. However, when the scene contains clouds and other bright features that produce saturated pixels, the LAI values can exceed 3.5.
}
\examples{
library(raster)
path_files <- system.file("extdata/", package="nightmares")
bands <- stack(list.files(path_files,".tif", full.names=TRUE))
x <- ref_oli(bands, sun.elev= 67.97)
evi <- EVI(x[[2]], x[[4]], x[[5]])
LAI(evi)

}
\references{
Boegh et al., 2002. Airborne Multi-spectral Data for Quantifying Leaf Area Index, Nitrogen Concentration and Photosynthetic Efficiency in Agriculture. Remote Sensing of Environment, 81(2-3), 179-193.
}
