% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hills.R, R/molaR_package.R
\docType{data}
\name{Hills}
\alias{Hills}
\alias{Hills.mesh}
\title{Hills surface mesh}
\format{
An object of class \code{"mesh3d"}

\code{Hills}: triangular mesh representing a sine-cosine plane.
}
\usage{
data(Hills)
}
\description{
Sample mesh created with the formula: `z=3cos(x/2)+3sin(y/2)

A triangular mesh representing a sine-cosine plane - called by data(Hills)
}
\examples{
data(Hills)
DNE_Output <- DNE(Hills)
DNE3d(DNE_Output)
}
\keyword{datasets}
