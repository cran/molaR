#' Plot results of a Slope analysis of a surface
#'
#' A function that produces a three-dimensional rendering of surface slope.
#' The Slope function will identify the slope of each mesh face. It must be
#' performed prior to using the Slope3d function.
#'
#' @param Slope_File An object that stores the output of the 
#' Slope function
#' @param colors String of colors to build the color gradient
#' @param maskNegatives Logical indicating whether or not to mask (in  black) 
#' negative slopes, or to reflect them into positive slopes
#' @param legend Logical indicating whether or not a legend
#' should be displayed
#' @param leftOffset numeric value between -1 and 1 setting the degree of
#' offset for the plotted surface to the left; larger values set further to left
#' while 0 is centered
#' @param fieldofview Passes an argument to par3d changing the field of
#' view in degrees of the resulting surface plot
#' @param fileName String indicating a name to save the plotted surface to as a
#' *.ply file; default of 'NA' will not save a file
#' @param binary Logical indicating whether or not the saved surface plot should
#' be binary, passed to vcgPlyWrite  
#'
#' @details This function creates a heat map on the mesh surface
#' corresponding to the slope of each face calculated by
#' the Slope function.
#' 
#' Colors are taken as a series inputs to define a color ramp and can be customized
#' indefinitely in value or order. The default is suggested as an intuitive display
#' of increasing color heat corresponding with steeper face slope.
#' 
#' The leftOffset value sets how far to the left the surface will appear, intended
#' to help avoid overlap with the legend. A value of 0 for this argument will center
#' the surface in the plotting window and negative values will shift it to the right.
#' 
#' fieldofview is set to a default of 0, which is an isometric parallel projection.
#' Raising it corresondingly increases the amount of obliquity used to render the
#' surface in the plotting window, up to a maximum of 179 degrees.
#' 
#' The plotted, colorized surface can be saved as a *.ply to the working directory
#' by changing the fileName argument from NA to a string (e.g., "SlopePlot"). The
#' resultant ply file can be opened and manipulated in other 3D visualizing programs,
#' such as MeshLab, but will NOT retain its legend (a background of the plotting window).
#' To retain the legend, the user is encouraged to utilize the snapshot3d function. The
#' binary argument saves a file in ascii format by default, which is supported by more
#' 3D visualization software than is binary. However, binary files will be considerably
#' smaller.
#'
#' @import
#' rgl grDevices graphics utils
#'
#' @importFrom
#' Rvcg vcgPlyWrite
#' 
#' @export
#' Slope3d
#'
#' @examples
#' Slope_output <- Slope(Tooth)
#' Slope3d(Slope_output)

Slope3d <- function(Slope_File,
                    colors=c("blue", "cornflowerblue", "green", "yellowgreen", "yellow", "orangered", "red"),
                    maskNegatives = TRUE, legend = TRUE, leftOffset = 1,
                    fieldofview = 0, fileName = NA, binary = FALSE)
{
  plyFile <- Slope_File$plyFile
	colorsfunc <- colorRamp(colors)
	color_range <- plyFile$Face_Slopes/90
	if(maskNegatives == FALSE) {
	  color_range[color_range > 1] <- abs(1-(color_range[color_range > 1]-1))
	  slope_colors <- colorsfunc(abs(color_range))
	}
	if(maskNegatives == TRUE) {
		slope_colors <- colorsfunc(color_range)
		slope_colors[is.na(slope_colors[,1]),] <- c(0,0,0)
	}
	slope_colors <- apply(slope_colors, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
	open3d()
	par3d(windowRect=c(100,100,900,900))
	shade3d(plyFile, color=slope_colors, meshColor='faces', shininess=100)
	if (legend == TRUE) {
		Input <- maskNegatives
		molaR_bgplot(Slope_Legend(colors=colors, maskNegatives=maskNegatives))
	}
	if (leftOffset > 1) {warning("Left offset greater than 1 may restrict mesh visibility")}
  if (leftOffset < -1) {warning("Left offset less than -1 may restrict mesh visibility")}
  rgl.viewpoint(fov = fieldofview)
  ZView <- par3d("observer")[3]
  XView <- leftOffset * ZView *0.05
  observer3d(XView, 0, ZView)
  if(!is.na(fileName)){
    if(!is.character(fileName)){stop("Enter a name for fileName")}
    if(substr(fileName, nchar(fileName)-3, nchar(fileName))!=".ply"){
      fileName <- paste(fileName, ".ply", sep="")
    }
    OutPly <- plyFile
    NewVertList <- plyFile$vb[,plyFile$it[1:length(plyFile$it)]]
    NewNormList <- plyFile$normals[,plyFile$it[1:length(plyFile$it)]]
    NewFaceList <- matrix(1:ncol(NewVertList), nrow=3)
    colormatrix <- matrix(rep(slope_colors, 3), nrow = 3, byrow = TRUE)
    NewColorList <- colormatrix[1:length(colormatrix)]
    OutPly$vb <- NewVertList
    OutPly$it <- NewFaceList
    OutPly$normals <- NewNormList
    OutPly$material$color <- NewColorList
    vcgPlyWrite(mesh=OutPly, filename = fileName, binary = binary)
    if(binary==FALSE){
      FileText <- readLines(con=paste(getwd(), "/", fileName, sep=""), warn = F)
      NewCom <- paste("comment Slope plot generated in molaR",
                      packageVersion("molaR"), "for", R.version.string)
      NewCom <- unlist(strsplit(NewCom, split='\n'))
      NewOut <- c(FileText[1:3], NewCom, FileText[(4):length(FileText)])
      writeLines(NewOut, con=paste(getwd(), "/", fileName, sep=""))
    }
  }
}
