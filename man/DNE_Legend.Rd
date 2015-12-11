% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DNE_Legend.R
\name{DNE_Legend}
\alias{DNE_Legend}
\title{Make legend for DNE3d plot}
\usage{
DNE_Legend(start, end, colors, DNELabels, scaled = F, edgeMask = F,
  outlierMask = F, logColors = F)
}
\arguments{
\item{start}{value for the legend to start with, i.e. bottome value}

\item{end}{value for the legend to end with, i.e. top value}

\item{colors}{range of values, defaulting to heat colors}

\item{DNELabels}{values for the labels}

\item{scaled}{logical indicating whether the values are scaled}

\item{edgeMask}{logical indicating whether of not edges are being masked
and that information to be included in the legend}

\item{outlierMask}{logical indicating whether outliers are masked}

\item{logColors}{logical indicating colors are on log scale}
}
\description{
plotting subfunction
}
\details{
This is an internal function which builds a better DNE plot legend

The legend will reflect the elements used in the plot. This is an internal
function. Users will have little need or call to interact with it.
}
