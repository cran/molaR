#' Plot results of a Slope analysis of a surface
#'
#' plotting function
#'
#' @param Slope_File An object that stores the output of the 
#' Slope function
#' @param colors A string of colors to build the color gradient
#' @param maskNegatives A logical indicating whether to mask 
#' the negative slopes or to reflect them into positive slopes
#' @param leftOffset numeric value between -1 and 1 setting 
#' the degree of offset for the plotted surface to the left. Larger 
#' values set further to right. 
#' @param fieldofview Passes an argument to par3d changing
#' the field of view in degrees of the resulting rgl  
#' @param legend Logical indicating whether or not a legend
#' shold be displayed
#'
#' @details This function creates a heat map on the mesh surface
#' corresponding to the slope of each face calculated by
#' the Slope function. Colors are customizable
#'
#' @import
#' rgl grDevices graphics utils
#' 
#' @export
#' Slope3d


Slope3d <- function(Slope_File, colors=c('red', 'orangered', 'orange', 'yellow', 'yellowgreen', 'green', 'cornflowerblue', 'blue'), maskNegatives=T, leftOffset=1, fieldofview=0, legend=T) 
{
	
	colorsfunc <- colorRamp(colors)
	color_range <- Slope_File$plyFile$Face_Slopes/90
	if(maskNegatives==F) {
		slope_colors <- colorsfunc(abs(color_range))
	}
	if(maskNegatives==T) {
		slope_colors <- colorsfunc(color_range)
		slope_colors[is.na(slope_colors[,1]),] <- c(0,0,0)
	}
	slope_colors <- apply(slope_colors, 1, function(x) rgb(x[1], x[2], x[3], maxColorValue=255))
	colormatrix <- rep(slope_colors, 3)
	colormatrix <- matrix(colormatrix, nrow=3, byrow=T)
	
	open3d()
	par3d(windowRect=c(100,100,900,900))
	
	shade3d(Slope_File$plyFile, color=colormatrix, shininess=100)
	if (legend==T) {
		Input <- maskNegatives
		molaR_bgplot(Slope_Legend(colors=colors, maskNegatives=maskNegatives))
  
	}
	
	
	  if (leftOffset > 1) {warning("Left offset greater than 1 may restrict mesh visibility")}
  if (leftOffset < -1) {warning("Left offset less than -1 may restrict mesh visibility")}
  rgl.viewpoint(fov = fieldofview)
  ZView <- par3d("observer")[3]
  XView <- leftOffset * ZView *0.05
  observer3d(XView, 0, ZView)
	
}