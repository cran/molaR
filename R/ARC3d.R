#' Plot Area Relative Curvature on Tooth Mesh 
#' 
#'
#' a molaR surface plotting function
#' 
#' @param ARC_object An object that stores the output of the `ARC()`
#' function
#' @param colors a concatenated string of colors for plotting different values of 
#' positive and negative curvature. 
#' @param main string indicating plot title. Defaults to empty
#' @param cex numeric value setting the relative size of the legend, default=1
#' @param cex.main numeric value setting the relative size of the plot title,
# default=2
#' @param legend Logical indicating whether or not a legend
#' shold be displayed. Default=T
#' @param fieldofview Passes an argument to `par3d()` changing the field of
#' view (in degrees) of the resulting 3D plot
#'
#' @details This function creates a surface map of the discarded DNE faces. 
#' DNE calculations typically discard the top 1 tenth of one percent of faces, 
#' associated with extreme pockets and broken parts of surfaces. DNE 
#' calculations also typically discard the boundary faces from the calculation, 
#' either on the basis of 2 vertices on the boundary, or at least one vertext on 
#' the boundary. concaveCol defaults to gray and therefore is turned off. When
#' an alternative color is provided, the function will identify the the areas of the 
#' tooth that are concave vs convex. 
#' 
#' Details of the other function arguments can be found in the DNE3d() description
#' and identical terms are organized to function the same way. 
#' 
#'
#'
#' @import
#' rgl grDevices graphics utils
#' 
#' @export
#' ARC3d
#'
#' @examples
#' ARC_output <- ARC(Tooth)
#' ARC3d(ARC_output)

ARC3d <- function(ARC_object, main='', cex.main=2, cex=1, colors=c('darkblue', 'blue', 'powderblue', 'gray', 'gray', 'tan', 'orange', 'darkorange1'), fieldofview=0, legend=T) {
	
	
HighQs <- quantile(ARC_object$Mean_Face_Curvature[which(ARC_object$Mean_Face_Curvature>=0)])
LowQs <- quantile(ARC_object$Mean_Face_Curvature[which(ARC_object$Mean_Face_Curvature<0)])


colorsref <- ARC_object$Mean_Face_Curvature
colors1 <- vector(mode='character', length=length(colorsref))

colors1[which(colorsref<=LowQs[2])] <- colors[1]
colors1[which(colorsref<=LowQs[3] & colorsref>LowQs[2])] <- colors[2]
colors1[which(colorsref<=LowQs[4] & colorsref>LowQs[3])] <- colors[3]
colors1[which(colorsref<0 & colorsref>LowQs[4])] <- colors[4]
colors1[which(colorsref>=HighQs[4])] <- colors[8]
colors1[which(colorsref<HighQs[4] & colorsref>=HighQs[3])] <- colors[7]
colors1[which(colorsref<HighQs[3] & colorsref>=HighQs[2])] <- colors[6]
colors1[which(colorsref>=0 & colorsref<HighQs[2])] <- colors[5]

 open3d()
  layout3d(matrix(c(1,2), byrow=T, nrow=2), heights=c(1,9))
  par3d(windowRect = c(100, 100, 800, 800/0.9))
  text3d(0,0,0, main, cex=cex.main, font=2)
  next3d()

shade3d(ARC_object$plyFile, color=colors1, meshColor='faces')
rgl.viewpoint(fov = fieldofview)
  
  if (legend == TRUE) {
    if(cex <= 0){stop("cex must be a positive number")}
    if(cex > 1.25){
      warning("cex greater than 1.25 may restrict legend visibility")
    }


    molaR_bgplot(ARC_Legend(colors=colors, size=1))}
    }
  
