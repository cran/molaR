#' Plot 3D and 2D areas of a mesh used to calculate relief index
#'
#' A function that plots a three-dimensional model of the mesh
#' surface and includes a footprint of the two-dimensional area for
#' visual comparison. 
#' 
#' @param RFI_Output An object that stores the output of the RFI
#' function
#' @param displace Moves the surface footprint Up, Down, or not at
#' all with None. Defaults to Down
#' @param SurfaceColor changes the color of the 3D surface mesh
#' @param FootColor changes color of the 2D surface footprint
#' @param fieldofview Passes an argument to par3d changing the
#' field of view in degrees of the resulting rgl window
#' @param Opacity adjusts the opacity of the 3D mesh
#' surface
#' @param legend Logical indicating whether or not to include a
#' legend of the colors chosen to represent the 3D surface and
#' footprint
#' @param legendScale cex style numeric relative scaling factor for the legend
#' @param leftOffset how numeric between -1 and 1 for which to offset the surface
#' relative to the legend. 
#' 
#' @details This function can help to visualize the three-dimensional and two
#' dimensional areas that are used in calculating the relief index of a surface by
#' displaying both at the same time. The RFI function must be performed first.
#' 
#' Opacity can be adjusted in a range from fully opaque (1) to fully
#' transparent (0) in order to help visualize the footprint. The vertical placement of
#' the footprint along the Z axis can be altered with displace depending on how the
#' user wishes to view the surface, or on the original mesh orientation.
#' 
#' fieldofview is set to a default of 0, which is an isometric projection. Increasing it
#' alters the degree of parallax in the perspective view, up to a maximum of 179
#' degrees.
#'
#' @import
#' rgl
#' 
#' @export
#' RFI3d



RFI3d <- function (RFI_Output, displace="Down", SurfaceColor="gray", FootColor="red",
                          Opacity=1, legend=F, legendScale=1, leftOffset=0, fieldofview=0)
{
  if(leftOffset > 1){warning("Left offset greater than 1 will restrict mesh visibility")}
  if(leftOffset < -1){warning("Left offset less than -1 will restrict mesh visibility")}
  leftOffset <- leftOffset * 0.2
  plyFile <- RFI_Output$plyFile
  Vertices <- plyFile$vb
  x <- Vertices[1,] - mean(Vertices[1,])
  y <- Vertices[2,] - mean(Vertices[2,])
  z <- Vertices[3,] - mean(Vertices[3,])
  n <- rep(1, length(Vertices))
  Shifted <- as.matrix(rbind(x, y, z, n))
  ShiftedPly <- plyFile
  ShiftedPly$vb <- Shifted
  FootColor = FootColor
  open3d()
  par3d(windowRect = c(100, 100, 900, 900))
  if(legend == T){
    textSizeFactor <- 1.75*legendScale
    lineSizeFactor <- 2*legendScale
    legSizeFactor <- legendScale
    lastRgl <- length(rgl.dev.list())
    isQuartz <- names(rgl.dev.list()[lastRgl])=="glX"
    if(isQuartz==FALSE){
      bgplot3d(RFI_Legend(surfCol = SurfaceColor, footCol = FootColor, lineSize = lineSizeFactor,
                          textSize = textSizeFactor, legSize = legSizeFactor, opac = Opacity))
    }
    if(isQuartz==TRUE){
      textSizeFactor <- 0.5*textSizeFactor
      lineSizeFactor <- 0.7*lineSizeFactor
      legSizeFactor <- 0.7*legSizeFactor
      bgplot3d_XQuartz(RFI_Legend(surfCol = SurfaceColor, footCol = FootColor,
                                  lineSize = lineSizeFactor, textSize = textSizeFactor,
                                  legSize = legSizeFactor, opac = Opacity))
    }
  }
  shade3d(ShiftedPly, color = SurfaceColor, alpha = Opacity)
  if(displace!="Down" && displace!="Up" && displace!="None"){
    warning("Displace parameter must be set to either 'Up', 'Down', or 'None'.")
  }
  FootprintPts <- RFI_Output$Flattened_Pts
  xy <- FootprintPts[, 1:2]
  MeshHeight <- max(plyFile$vb[3,])-min(plyFile$vb[3,])
  displaceDist <- 0.9 * MeshHeight
  if(displace == "Up"){
    zpts <- FootprintPts[, 3] + displaceDist
    xyz <- cbind(xy, zpts)
    FootprintVertices <- cbind(xyz, rep(1, length(zpts)))
    FootprintVertices <- t(FootprintVertices)
    triangles <- t(RFI_Output$Footprint_Triangles)
    Footprint <- list(vb = FootprintVertices, it = triangles, 
                      primitivetype = "triangle", material = NULL)
    class(Footprint) <- c("mesh3d", "shape3d")
    shade3d(Footprint, color = FootColor)
  }
  if (displace == "Down") {
    zpts <- FootprintPts[, 3] - displaceDist
    xyz <- cbind(xy, zpts)
    FootprintVertices <- cbind(xyz, rep(1, length(zpts)))
    FootprintVertices <- t(FootprintVertices)
    triangles <- t(RFI_Output$Footprint_Triangles)
    Footprint <- list(vb = FootprintVertices, it = triangles, 
                      primitivetype = "triangle", material = NULL)
    class(Footprint) <- c("mesh3d", "shape3d")
    shade3d(Footprint, color = FootColor)
  }
  if (displace == "None") {
    zpts <- FootprintPts[, 3]
    xyz <- cbind(xy, zpts)
    FootprintVertices <- cbind(xyz, rep(1, length(zpts)))
    FootprintVertices <- t(FootprintVertices)
    triangles <- t(RFI_Output$Footprint_Triangles)
    Footprint <- list(vb = FootprintVertices, it = triangles, 
                      primitivetype = "triangle", material = NULL)
    class(Footprint) <- c("mesh3d", "shape3d")
    shade3d(Footprint, color = FootColor)
  }
  rgl.viewpoint(fov=fieldofview)
  ZView <- par3d('observer')[3]
  XMin <- abs(min(x))
  XMax <- abs(max(x))
  if(XMin > XMax){
    XView <- leftOffset*XMin
  }
  if(XMax >= XMin){
    XView <- leftOffset*XMax
  }
  observer3d(XView, 0, ZView)
}