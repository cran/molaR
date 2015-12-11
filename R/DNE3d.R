#' Plot results of a DNE analysis of a surface 
#' 
#' plotting function
#' 
#' @param DNE_File An object that stores the output of the DNE
#' function
#' @param setRange User-defined range for plotting color scheme, see
#' Details
#' @param edgeMask Logical that colors edge faces black to indicate their
#' lack of contribution to the total Dirichlet normal energy
#' @param outlierMask Logical that colors outlier faces dark gray to
#' indicate their lack of contribution to the Dirichlet normal energy
#' @param logColors Logical that log transforms the color scheme
#' @param showEdgePts Logical that highlights the edge vertices in red
#' to indicate their lack of contribution ot the total Dirichlet normal energy
#' @param fieldofview Passes an argument to par3d changing the field of
#' view in degrees of the resulting rgl 
#' @param legend Logical indicating whether or not a legend
#' shold be displayed
#' 
#' @details This function creates a heat map on the mesh surface
#' corresponding to the Dirichlet normal energy of each face calculated by
#' the DNE function. Hottest colors represent highest normal energy
#' values
#'
#' Dirichlet normal energies for the faces of a mesh surface tend to be
#' positively skewed, with a small proportion of the faces contributing
#' much of the total energy for the surface. When logColors is enabled the
#' function colorizes based on the log transformed Dirichlet normal
#' energies, allowing for finer resolution between faces near the mode of
#' the energy per face distribution. Disabling logColors will display the
#' untransformed Dirichlet normal energies.
#'
#' The legend will update to reflect the other arguments chosen by the
#' user. Colors currently display in the legend in bins, however the colors
#' used in the displayed mesh surface are on a continuum. Ideally, the
#' legend should reflect a continuous stretch of color from the lowest
#' calculated Dirichlet normal energy to the highest. Future versions will
#' adjust the legend to this more intuitive display.
#'
#' By default, the function sets the lowest Dirichlet normal energy
#' calculated among all faces to a cool color and the highest normal energy
#' calculated among all faces to red, and then colors the remaining faces
#' on a continuous color spectrum between these two end points using
#' either absolute or log transformed Dirichlet normal energy values
#' (depending on the status of logColors). Since the scale is relative to the
#' energies of the input surface, visual comparisons cannot directly be
#' made between  multiple plots of different surfaces. The setRange
#' argument allows users to define the minimum and maximum of the
#' plotting color scheme and use it in multiple plots. This enables the
#' direct comparison of different surfaces to one another with red equal to
#' the user-defined maximum and a cool color equal to the user-defined
#' minimum. The user should choose reasonable bounds for the
#' maximum and minimum that are near the maximum and minimum
#' Dirichlet normal energies calculated for their surfaces. setRange will
#' not accept negative values.
#' 
#' fieldofview is set to a default of 0, which is an isometric projection.
#' Increasing it alters the degree of parallax in the perspective view, up to
#' a maximum of 179 degrees.
#'
#'
#' @import
#' rgl grDevices graphics utils
#' 
#' @export
#' DNE3d

DNE3d <- function(DNE_File, setRange = c(0,0), edgeMask=TRUE, outlierMask=TRUE, logColors=TRUE,
                        showEdgePts=FALSE, fieldofview=0, legend=TRUE){

  DNEs <- DNE_File$Face_Values$DNE_Values*DNE_File$Face_Values$Face_Areas
  
  # Obtain plotting color scheme relative to DNE analysis accepted as input (setRange disabled):
  if(setRange[1]==0 && setRange[2]==0){
    color_range <- DNEs*(1/max(DNEs))  # Standardizes range of colors to between 0 and 1, regardless of max DNE value
    color_range <- (color_range*-1+1)*0.575  # Reverse the color range, so that highest DNE = hottest color in hsv scale
    # The 0.5 scalar makes the lowest DNE values a light blue. Can be altered to preference
    DNE_colors <- hsv(color_range)  # Use the scaled DNE values per face to obtain colors in the hsv spectrum
    legend_labels <- round(seq(max(DNEs), min(DNEs), l=10), digits = 4)
    
    # Alternative coloration scheme, log transforming DNE values to create more contrast in plot:
    if(logColors==TRUE){
      zeroes <- which(DNEs==0)  # Identify and eliminate zeroes from the list to avoid log(0) = -Inf
      DNEs[zeroes] <- 0.000001
      logDNEs <- log(DNEs)
      logDNEs <- logDNEs+abs(min(logDNEs))  # Make all DNE values positive
      color_range <- logDNEs*(1/max(logDNEs))  # Standardizes range of colors to between 0 and 1, regardless of max DNE value
      color_range <- (color_range*-1+1)*0.575  # Reverse the color range, so that highest DNE = hottest color in hsv scale
      # The scalar is increased here to make a greater color range for the log transformation. Can be altered to preference
      DNE_colors <- hsv(color_range)  # Use the scaled DNE values per face to obtain colors in the hsv spectrum
      log_legend_labels <- round(exp(seq(log(max(DNEs)), log(min(DNEs)), l=10)), digits=4)
    }
  }

  # Obtain plotting color scheme based on end points selected by user (setRange enabled):
  if(setRange[1]!=0 | setRange[2]!=0){
    setMax <- max(setRange)
    setMin <- min(setRange)
    # Alert the user if the range boundaries are incompatable with plotting
    if(setMax<max(DNEs)){warning("setRange max is less than highest calculated face energy")}
    if(setMin>min(DNEs)){warning("setRange min is greater than lowest calculated face energy")}
    if(setMin<0){warning("Negative values not accepted for face energy range")}
    color_range <- DNEs*(1/setMax)  # Standaridzes range of colors to between 0 and 1, with user-defined max at 1
    color_range <-(color_range*-1+1)*0.575  # Reverse the color range, so that the highest DNE = hottest color in the hsv scale
    DNE_colors <- hsv(color_range)  # Use the scaled DNE values per face to obtain colors in the hsv spectrum
    legend_labels <- round(seq(setMax, setMin, l=10), digits=4)
    
    # Alternative coloration scheme, log transforming DNE values to create more contrast in plot:
    if(logColors==TRUE){
      zeroes <- which(DNEs==0)  # Identify and eliminate zeroes from the list to avoid log(0) = -Inf
      DNEs[zeroes] <- 0.000001
      logDNEs <- log(DNEs)
      Top <- log(setMax) + abs(min(logDNEs))  # Necessary in making adjustments relative to user-defined maximum, below
      logDNEs <- logDNEs+abs(min(logDNEs))  # Make all DNE values positive
      # Making DNE's positive can push values beyond user-defined max. Correction here:
      if(max(logDNEs)>setMax){
        Adjustment <- max(logDNEs)/Top  # An adjustment value that accounts for the difference between the max log(DNE) and the user-defined max
        color_range <- logDNEs*(1/max(logDNEs))*Adjustment  # Standardizes range of colors to between 0 and 1, relative to user-defined maximum
        color_range <- (color_range*-1+1)*0.575  # Reverse the color range, so that highest DNE = hottest color in hsv scale
        DNE_colors <- hsv(color_range)  # Use the scaled DNE values per face to obtain colors in the hsv spectrum
      }
      if(max(logDNEs)<=setMax){
        color_range <- logDNEs*(1/setMax)  # Standaridzes range of colors to between 0 and 1, with user-defined max at 1  
        color_range <- (color_range*-1+1)*0.675  # Reverse the color range, so that highest DNE = hottest color in hsv scale
        # The scalar is increased here to make a greater color range for the log transformation. Can be altered to preference
        DNE_colors <- hsv(color_range)
      }
      # If user defined minimum was 0, must be adjusted to small positive value to avoid log(0) = -Inf
      if(setMin==0){
        setMin <- 0.000001
      }
      log_legend_labels <- round(exp(seq(log(setMax), log(setMin), l=10)), digits=4)
    }
  }

  # Mask the faces that are not included in surface DNE calculation (optional):
  if(edgeMask==TRUE){
    edges <- as.numeric(rownames(DNE_File$Edge_Values))  # Identify which faces are edges, from original DNE calculation
    DNE_colors[edges] <- "#000000"  # Paint those faces black
  }
  if(outlierMask==TRUE){
    outliers <- as.numeric(rownames(DNE_File$Outliers))  # Identify which faces are outliers, from original DNE calculation
    DNE_colors[outliers] <- "#505050"  # Paint those faces dark grey
  }
  
  # Basic plotting necessities:
  open3d()
  par3d(windowRect=c(100,100,900,900))  # Create a larger window for good legend rendering
  rgl.viewpoint(fov=fieldofview)  # Alter the field of view to preference. Default is to orthogonal view
  
  # Create legend of DNE value range.
  # Create an appropriate legend based on user-defined options:
  color_range <- sort(color_range)
  if(legend==TRUE){
    if(setRange[1]==0 && setRange[2]==0){
      if(logColors==TRUE){
        bgplot3d(
        DNE_Legend(start=0, end=0.575, colors=color_range, DNELabels=rev(log_legend_labels), scaled=F, edgeMask=edgeMask, outlierMask=outlierMask, logColors=logColors)
        )
      }
      if(logColors==FALSE){
        bgplot3d(
          DNE_Legend(start=0, end=0.575, colors=color_range, DNELabels=rev(legend_labels), scaled=F, edgeMask=edgeMask, outlierMask=outlierMask, logColors=logColors)
        )
      }
    }
    if(setRange[1]!=0 | setRange[2]!=0){
      if(logColors==TRUE){
        bgplot3d(
          DNE_Legend(start=0, end=0.575, DNELabels=rev(log_legend_labels), scaled=T, edgeMask=edgeMask, outlierMask=outlierMask, logColors=logColors)
        )
      }
      if(logColors==FALSE){
        bgplot3d(
          DNE_Legend(start=0, end=0.575, DNELabels=rev(legend_labels), scaled=T, edgeMask=edgeMask, outlierMask=outlierMask, logColors=logColors)
        )
      }
    }
  }
  
  # Repeat face colors into a 3 x n matrix to correctly color faces of a shaded 3d mesh
  colormatrix <- rep(DNE_colors, 3)
  colormatrix <- matrix(colormatrix, nrow=3, byrow=TRUE)
  plyFile <- DNE_File$plyFile
  shade3d(plyFile, color=colormatrix, shininess=110)  # Draws the mesh
  
  # Indicates edge vertices (optional):
  if(showEdgePts==TRUE){
    EdgeVert <- edge_vertices(plyFile)
    p <- t(plyFile$vb[,EdgeVert])
    points3d(x=p[,1], y=p[,2], z=p[,3], col="red", size=5)    
  }
}
