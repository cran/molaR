% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/molaR_Batch.R
\name{molaR_Batch}
\alias{molaR_Batch}
\title{Run molaR analyses on a batch of specimens}
\usage{
molaR_Batch(
  pathName = getwd(),
  fileName = "molaR_Batch.csv",
  DNE = TRUE,
  RFI = TRUE,
  OPCr = TRUE,
  OPC = FALSE,
  Slope = TRUE,
  details = TRUE,
  parameters = TRUE,
  ...
)
}
\arguments{
\item{pathName}{The path to the folder containing all ply surfaces to be
analyzed. Defaults to the working directory.}

\item{fileName}{Name for the output .csv file containing results and parameters}

\item{DNE}{Logical indicating whether or not to perform the DNE calculation}

\item{RFI}{Logical indicating whether or not to perform the RFI calculation}

\item{OPCr}{Logical indicating whether or not to perform the OPCr calculation}

\item{OPC}{Logical indicating whether or not to perform the OPC calculation}

\item{Slope}{Logical indicating whether or not to perform the Slope calculation}

\item{details}{Logical indicating whether or not to save additional output from
some of the topographic analyses}

\item{parameters}{Logical indicating whether or not to save the list of analysis
parameters used in the batch run}

\item{...}{Additional arguments passed to the topographic analysis functions.
See Details.}
}
\description{
A function that automates molaR analyses on multiple specimens. Several
different analyses can be performed on each surface, with specifications for
analysis parameters.
}
\details{
This function allows a user to set the analyses from molaR they want
to run on a batch of ply files. Output is saved to a csv file. By default, the
batch function will perform specified analyses on all ply files in the working
directory. A different folder can be specified with \code{pathName}. Output saves
as .csv to the folder that contains the analyzed ply files.

Any of the default arguments of the various topographic analysis functions can
be modified for the batch by specifying them when calling \code{molaR_Batch}, e.g.,
the DNE \code{kappa} value can be changed to 'X' by specifying \code{kappa = X}. Users
are \strong{strongly} encouraged to review the documentation for \code{\link[=DNE]{DNE()}},
\code{\link[=RFI]{RFI()}}, \code{\link[=OPCr]{OPCr()}}, \code{\link[=OPC]{OPC()}},
and \code{\link[=Slope]{Slope()}} and to understand the effects of alterations before making
changes. A recommended practice for analyzing RFI in a batch of specimens is to enable
\code{findAlpha = TRUE} given that the ideal \code{alpha} value is likely to vary among different
specimens. However, this will increase calculation time (see documentation for
\code{\link[=RFI]{RFI}}).

By default, the batch output will retain some additional details of the analysis.
These include, in the case of DNE: convex and concave DNE values, convex and
concave surface areas; in the case of RFI: 3D and 2D surface areas and analysis
\code{alpha} values; in the case of OPCr: the surface OPC value calculated at each
rotation; and in the case of OPC: the patch count for each bin. These results
will be discarded and only the final result of each topographic analysis will
be retained if \code{details = FALSE}.

The function will save a list of all parameters used in all batch analyses
to the output .csv file, below the results. This can be suppressed with
\code{parameters = FALSE}, but is recommended as a check on how analyses were
performed when returning to results in the future. If the function is assigned
to an object in R, the parameters are not included in the resultant data.frame,
but will still be included in the .csv file by default.

Note that batch processing updates will not display by default if using RGui
for Windows. Disable Misc -> Buffered output (Ctrl+W) if you wish to view
batch processing progress in RGui for Windows.
}
