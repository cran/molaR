#' Plot 3D and 2D areas of a mesh used to calculate relief index
#'
#' A function that plots a three-dimensional model of the mesh
#' surface and includes a footprint of the two-dimensional area for
#' visual comparison. 
#' 
#' @param RFI_File An object that stores the output of the RFI
#' function
#' @param displacement Numeric that moves the surface footprint some
#' proportion of the height of the mesh. 0 is in the vertical center of the
#' surface, negative values displace the footprint downward. 
#' @param SurfaceColor String that controls the color of the 3D surface mesh
#' @param FootColor String that controls the color of the 2D surface footprint
#' @param FootPts Logical indicating whether to plot the
#' flattened points of the footprint from the original ply file
#' @param FootPtsColor Color of the plotted footprint points if `FootPts = TRUE`
#' @param Opacity Numeric that adjusts the opacity of the 3D mesh surface
#' @param legend Logical indicating whether or not to include a
#' legend of the colors chosen to represent the 3D surface and
#' footprint
#' @param main String indicating plot title
#' @param cex Numeric setting the relative size of the legend and title
#' @param leftOffset Numeric between -1 and 1 setting the amount of offset for
#' the plotted surface to the left. Larger values push surface farther to right.
#' @param fieldofview Passes an argument to `par3d()` changing the field of
#' view in degrees of the resulting surface plot
#' @param fileName String indicating a name to save the plotted surface to as a
#' *.ply file; default of 'NA' will not save a file
#' @param binary Logical indicating whether or not the saved surface plot should
#' be binary, passed to `vcgPlyWrite()`
#' 
#' @details This function can help to visualize the three-dimensional and two
#' dimensional areas that are used in calculating the relief index of a surface by
#' displaying both at the same time. The RFI function must be performed first.
#' 
#' `Opacity` can be adjusted in a range from fully opaque (`1`) to fully
#' transparent (`0`) in order to help visualize the footprint. The vertical
#' placement of the footprint along the Z axis can be altered with `displacement`,
#' depending on how the user wishes to view the surface, or on the original
#' mesh orientation.
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
#' by changing the `fileName` argument from `NA` to a string (e.g., "RFIPlot"). The
#' resultant ply file can be opened and manipulated in other 3D visualizing programs,
#' such as \href{https://www.meshlab.net/}{MeshLab}, but will **NOT** retain its legend
#' (a background of the plotting window). To retain the legend, the user is 
#' encouraged to utilize the function 'snapshot3d()' in the rgl package. (see \code{\link[rgl:rgl.snapshot]{rgl::rgl.snapshot()}}) 
#' The `binary` argument saves a file in ascii format by default, which is supported by 
#' more 3D visualization software than is binary. However, binary files will be
#' considerably smaller.
#'
#' @import
#' rgl
#' 
#' @export
#' RFI3d
#'
#' @examples
#' RFI_File <- RFI(Tooth, alpha=0.5)
#' RFI3d(RFI_File)


RFI3d <- function (RFI_File, displacement = -1.9, SurfaceColor = "gray",
                   FootColor = "red", FootPts = FALSE, FootPtsColor = "black",
                   Opacity = 1, legend = F, main = '', cex = 1, leftOffset = 0,
                   fieldofview = 0, fileName=NA, binary=FALSE) 
{
  plyFile <- RFI_File$plyFile
  Vertices <- plyFile$vb
  x <- Vertices[1, ] - mean(Vertices[1, ])
  y <- Vertices[2, ] - mean(Vertices[2, ])
  z <- Vertices[3, ] - mean(Vertices[3, ])
  n <- rep(1, length(Vertices))
  Shifted <- as.matrix(rbind(x, y, z, n))
  ShiftedPly <- plyFile
  ShiftedPly$vb <- Shifted
  open3d()
  layout3d(matrix(c(1,2), byrow=T, nrow=2), heights=c(1,9))
  par3d(windowRect = c(100, 100, 800, 800/.9))
  text3d(0,0,0, main, cex=cex*2.5, font=2)
  next3d()
  shade3d(ShiftedPly, meshColor = 'faces', color = SurfaceColor,
          alpha = Opacity, shininess=110)
  if (legend == T) {
    if(cex <= 0){stop("cex must be a positive number")}
    if(cex > 1.25){
      warning("cex greater than 1.25 will restrict legend visibility")
    }
    molaR_bgplot(RFI_Legend(surfCol = SurfaceColor, footCol = FootColor,
                                   size = cex, opac = Opacity))
  }
  FootprintPts <- RFI_File$Flattened_Pts
  MeshHeight <- abs(max(plyFile$vb[3, ]) - min(plyFile$vb[3, ]))
  displaceDist <- displacement*0.5*MeshHeight
  zpts <- FootprintPts[,3] + displaceDist
  xyz <- cbind(FootprintPts[,1:2], zpts)
  if(FootPts==TRUE){
    points3d(xyz, color=FootPtsColor)
  }
  FootprintVertices <- t(cbind(xyz, rep(1, length(zpts))))
  triangles <- t(RFI_File$Footprint_Triangles)
  Footprint <- list(vb=FootprintVertices, it=triangles,
                    primitivetype="triangle", material=NULL)
  class(Footprint) <- c("mesh3d", "shape3d")
  shade3d(Footprint, meshColor = 'faces', color=FootColor)
  rgl.viewpoint(fov = fieldofview)
  if (leftOffset > 1) {warning("Left offset greater than 1 may restrict mesh visibility")}
  if (leftOffset < -1) {warning("Left offset less than -1 may restrict mesh visibility")}
  ZView <- par3d("observer")[3]
  XView <- leftOffset * ZView *0.055
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
    colormatrix <- matrix(rep(colormatrix, 3), nrow = 3, byrow = TRUE)
    NewColorList <- colormatrix[1:length(colormatrix)]
    OutPly$vb <- NewVertList
    OutPly$it <- NewFaceList
    OutPly$normals <- NewNormList
    OutPly$material$color <- NewColorList
    vcgPlyWrite(mesh=OutPly, filename = fileName, binary = binary)
    if(binary==FALSE){
      FileText <- readLines(con=paste(getwd(), "/", fileName, sep=""), warn = F)
      NewCom <- paste("comment OPC plot generated in molaR",
                      packageVersion("molaR"), "for", R.version.string)
      NewCom <- unlist(strsplit(NewCom, split='\n'))
      NewOut <- c(FileText[1:3], NewCom, FileText[(4):length(FileText)])
      writeLines(NewOut, con=paste(getwd(), "/", fileName, sep=""))
    }
  }
}