% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DNEpie.R
\name{DNEpie}
\alias{DNEpie}
\title{Plot advanced results of a DNE surface analysis}
\usage{
DNEpie(
  DNE_File,
  main = "",
  type = "area",
  convexCol = "hotpink",
  concaveCol = "deepskyblue"
)
}
\arguments{
\item{DNE_File}{An object that stores the output of the \code{DNE()}
function}

\item{main}{User's title for the plot}

\item{type}{string determine which parameters to plot. Default='DNE'
also accepts 'area' to plot pie charts of the area.}

\item{convexCol}{Color for the portion of the pie chart representing convex
contribution. Default='hotpink'. Accepts any color keyword.}

\item{concaveCol}{Color for the portion of the pie chart representing concave
contribution. Default='deepskyblue'. Accepts any color keyword.}
}
\description{
Plot advanced results of a DNE surface analysis
}
\details{
This function creates a pie chart of the total area or DNE of the surface
originating from the concave or convex portions of the surface. The function
defaults to plotting surface area, however, relative proportion of total DNE from the
concave and convex portions of the surface can be plotted by calling \code{type='DNE'}.
Colors can be customized by altering the \code{convexCol} and \code{concaveCol} arguments.
}
\examples{
DNE_output <- DNE(Tooth)
DNEpie(DNE_output)
}
