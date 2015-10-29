#' Plot results of OPC analysis of a surface
#'
#' A function that produces a three-dimensional rendering of face
#' orientation on a surface. The OPC function will identify the
#' orientations of mesh faces and assign them to patches. It must be
#' performed prior to using the OPC3d function. 
#'
#' @param OPC_Output_Object An object that stores the output of
#' the OPC function 
#' @param fieldofview Passes an argument to par3d changing the
#' field of view in dregrees of the resulting rgl window
#' @param legend Logical indicating whether or not a legend should
#' be displayed
#' @param colors Allows the user to change the colors filled in for
#' each directional bin
#' 
#' @details This function will assign a uniform color to all faces on the mesh
#' surface that share one of the 8 orientations identified by the OPC function. The
#' function returns a colored shade3d of the mesh so that patches can be visually
#' inspected. Future versions will include the option to black out patches not
#' included in the orientation patch count.
#' 
#' fieldofview is set to a default of 0, which is an isometric projection. Increasing it
#' alters the degree of parallax in the perspective view, up to a maximum of 179
#' degrees.

#' colors will support any vector of 8 colors, in any coloration scheme. Default
#' draws from the hsv color space to evenly space color information, however user
#' can supply a list of RGB values, character strings, or integers in place.
#'
#' @export
#' OPC3d()


OPC3d <- function(OPC_Output_Object, fieldofview=0, legend=TRUE,
	colors=hsv(h=(seq(10, 290, 35)/360), s=.8, v=.9)) {
  plyFile <- OPC_Output_Object$plyFile
  fieldofview=fieldofview
  bins <- plyFile$Directional_Bins
  Ones <- which(bins==1)
  bins[Ones] <- colors[1]
  Twos <- which(bins==2)
  bins[Twos] <- colors[2]
  Threes <- which(bins==3)
  bins[Threes] <- colors[3]
  Fours <- which(bins==4)
  bins[Fours] <- colors[4]
  Fives <- which(bins==5)
  bins[Fives] <- colors[5]
  Sixes <- which(bins==6)
  bins[Sixes] <- colors[6]
  Sevens <- which(bins==7)
  bins[Sevens] <- colors[7]
  Eights <- which(bins==8)
  bins[Eights] <- colors[8]
  colormatrix <- rep(bins, 3)
  colormatrix <- matrix(colormatrix, nrow=3, byrow=T)
  
  open3d()
  par3d(windowRect=c(100,100,900,900))  # Create a larger window for good legend rendering
  rgl.viewpoint(fov=fieldofview)  # Alter the field of view to preference. Default is to orthogonal view
  
  if(legend==TRUE){
    legend3d(x='right', legend=c(1:8), fill=colors,
            title="Orientations\nby Bin", bty='n', cex=1.75)
  }
  
  shade3d(plyFile, color=colormatrix)
  
}

