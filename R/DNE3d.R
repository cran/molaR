#' Plot results of a DNE analysis of a surface 
#' 
#' a molaR surface plotting function
#' 
#' @param DNE_File An object that stores the output of the `DNE()`
#' function
#' @param setMax User-defined upper range for plotting color scheme, see
#' Details
#' @param logColors Logical that log transforms the color scheme
#' @param signColor Logical indicating whether or not to plot by 
#' concavity vs convexity. Plotting by curve orientation is the default. 
#' @param main String indicating plot title
#' @param cex Numeric setting the relative size of the legend
#' @param cex.main Numeric setting the size of the title
#' @param legend Logical indicating whether or not a legend should be displayed
#' @param leftOffset Numeric between -1 and 1 setting the amount of offset for
#' the plotted surface to the left. Larger values push surface farther to right. 
#' @param fieldofview Passes an argument to `par3d()` changing the field of
#' view (in degrees) of the resulting 3D plot
#' @param fileName String indicating a name to save the plotted surface to as a
#' *.ply file; default of 'NA' will not save a file
#' @param binary Logical indicating whether or not the saved surface plot should
#' be binary, passed to `vcgPlyWrite()`
#'
#' @details This function creates a heat map on the mesh surface
#' corresponding to the Dirichlet energy density of each face calculated by
#' the `DNE()` function. Hottest colors represent highest normal energy
#' values.
#'
#' Dirichlet energy densities for the faces of a mesh surface tend to be
#' positively skewed, with a small proportion of the faces contributing
#' most of the total energy for the surface. When `logColors` is enabled, the
#' function colorizes based on the log-transformed Dirichlet energy
#' densities, allowing for finer resolution between faces near the mode of
#' the energy per face distribution. Disabling `logColors` will display the
#' un-transformed Dirichlet energy densities.
#'
#' The legend will update to reflect the other arguments chosen by the
#' user. By default, the function sets the lowest Dirichlet energy density
#' calculated among all faces to a cool color and the absolute highest normal
#' energy calculated among all faces to a hot color, and then colors the remaining
#' faces on a continuous color spectrum between these two end points using
#' either absolute or log transformed Dirichlet energy density values
#' (depending on the status of `logColors`). Since the scale is relative to the
#' energies of the input surface, visual comparisons cannot directly be
#' made between multiple plots of different surfaces.
#' 
#' The `setMax` argument allows users to define the maximum of the
#' plotting color scheme for use across multiple plots. This enables the
#' direct comparison of different surfaces to one another with red equal to
#' the user-defined maximum and a cool color equal to the minimum. The user 
#' should choose a reasonable upper bound for the maximum. `setMax` will not 
#' accept negative values. If there are faces with Dirichlet normal energy
#' values higher than the `setMax` value, these faces are marked with the
#' highest possible color.
#'
#' The logical `signColor` colors the surface with two separate gradients,
#' one for the convex and one for the concave faces (curvature sign). By
#' default, the plot now makes this distinction.
#' 
#' A title can be added to the plot by supplying a character string to the `main`
#' argument. Title and legend size are controlled with the `cex` argument,
#' analogous to that in the default R graphics device.
#' 
#' The `leftOffset` value sets how far to the left the surface will plot, intended
#' to help avoid overlap with the legend. Value of 0 will center the surface and
#' should be invoked if the `legend` argument is disabled. Higher values will push
#' the surface farther left and negative values will push it to the right. It is
#' recommended that these values be restricted between -1 and 1 to avoid plotting
#' the surface outside of the rgl window.
#' 
#' `fieldofview` is set to a default of 0, which is an isometric projection.
#' Increasing it alters the degree of parallax in the perspective view, up to
#' a maximum of 179 degrees (see \code{\link[rgl:par3d]{rgl::par3d()}}).
#' 
#' The plotted, colorized surface can be saved as a *.ply to the working directory
#' by changing the `fileName` argument from `NA` to a string (e.g., "DNEPlot"). The
#' resultant ply file can be opened and manipulated in other 3D visualizing programs,
#' such as \href{https://www.meshlab.net/}{MeshLab}, but will **NOT** retain its legend
#' (a background of the plotting window). To retain the legend, the user is 
#' encouraged to utilize the function 'snapshot3d()' in the rgl package. (see \code{\link[rgl:rgl.snapshot]{rgl::rgl.snapshot()}}) 
#' The `binary` argument saves a file in ascii format by default, which is supported by 
#' more 3D visualization software than is binary. However, binary files will be
#' considerably smaller.
#'
#' @import
#' rgl grDevices graphics utils
#' 
#' @export
#' DNE3d
#'
#' @examples
#' DNE_output <- DNE(Tooth)
#' DNE3d(DNE_output)

DNE3d <- function (DNE_File, setMax = 0, logColors = TRUE, signColor = TRUE,
                   main = '', cex = 1, cex.main=2, legend = TRUE, leftOffset = 1,
                   fieldofview = 0, fileName=NA, binary = FALSE) 
{
  plyFile <- DNE_File$plyFile
  DNEs <- DNE_File$Face_Values$Dirichlet_Energy_Densities*DNE_File$Face_Values$Face_Areas
  Kappa <- DNE_File$Kappa
  kappas <- DNE_File$Face_Values$Kappa_Values
  neg <- which(kappas<Kappa)
  userMax <- setMax
  if (setMax < 0) {stop("Negative values not accepted for face energy maximum")}
  if(setMax == 0) {setMax <- max(DNEs)}
  if(setMax < max(DNEs)){
  	DNEs[which(DNEs > setMax)] <- setMax
  }
  
  if (logColors == TRUE) {
    logcut <- 1e-6;
    zs <- which(DNEs < logcut)
    DNEs[zs] <- logcut
    logDNEs <- log(DNEs)
    shift <- -1 * min(logDNEs)
    logDNEs <- logDNEs + shift
    Top <- log(setMax) + shift
    color_range <- logDNEs / Top
  }
  if (logColors==FALSE) {
    color_range <- DNEs / setMax
  }
  
  signed_range <- color_range
  signed_range[neg] <- -1 * color_range[neg]
  signed_range <- (signed_range + 1) / 2
     
  if (signColor==T) {
    DNE_colors <- signedcolor.gradient(interpolate_faces_to_vertices(signed_range, plyFile))
  }
  if (signColor==F) {
  	DNE_colors <- ccolor.gradient(interpolate_faces_to_vertices(color_range, plyFile))
  }
  
  open3d()
  layout3d(matrix(c(1,2), byrow=T, nrow=2), heights=c(1,9))
  par3d(windowRect = c(100, 100, 800, 800/0.9))
  text3d(0,0,0, main, cex=cex.main, font=2)
  next3d()
  shade3d(plyFile, color = DNE_colors, meshColor='vertices', shininess = 110)
  rgl.viewpoint(fov = fieldofview)
  
  if (legend == TRUE) {
    if(cex <= 0){stop("cex must be a positive number")}
    if(cex > 1.25){
      warning("cex greater than 1.25 may restrict legend visibility")
    }
    scaled <- FALSE
    if(userMax != 0){scaled <- TRUE}
    if (logColors == TRUE) {
      x <- seq(log(setMax), log(logcut), l=5)
      x <- exp(x)
      x <- c(x,rev(-x))
    	if(signColor==T) {
    	  legend_labels <- rev(x)
    	}
    	if(signColor==F) {
    	  y <- seq(log(setMax), log(logcut), l=10)
    	  y <- exp(y)
    	  legend_labels <- rev(y)	
    	}
    }
    if (logColors == FALSE) {
      x <- seq(setMax, 0, l=5)
      x <- c(x, rev(-x))
    	if(signColor==T) {
    	  legend_labels <- rev(x)
    	}
    	if(signColor==F) {
    	  y <- seq(setMax, 0, l=10)
    	  legend_labels <- rev(y)	
    	}
    }
    molaR_bgplot(DNE_Legend(DNELabels = legend_labels, scaled = scaled, 
                            logColors = logColors, size = cex, 
                            signColor=signColor))
  }
  
  if (leftOffset > 1) {warning("Left offset greater than 1 may restrict mesh visibility")}
  if (leftOffset < -1) {warning("Left offset less than -1 may restrict mesh visibility")}
  ZView <- par3d("observer")[3]
  XView <- leftOffset * ZView *0.05
  observer3d(XView, 0, ZView)
  
  if (!is.na(fileName)) {
    if (!is.character(fileName)) {stop("Enter a name for fileName")}
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply") {
      fileName <- paste(fileName, ".ply", sep = "")
    }
    plyFile$material$color <- DNE_colors
    vcgPlyWrite(mesh = plyFile, filename = fileName, binary = binary)
    if (binary == FALSE) {
      FileText <- readLines(con = paste(getwd(), "/", 
                                        fileName, sep = ""), warn = F)
      NewCom <- paste("comment DNE plot generated in molaR", 
                      packageVersion("molaR"), "for", R.version.string)
      NewCom <- unlist(strsplit(NewCom, split = "\n"))
      NewOut <- c(FileText[1:3], NewCom, FileText[(4):length(FileText)])
      writeLines(NewOut, con = paste(getwd(), "/", 
                                     fileName, sep = ""))
    }
  }
}
