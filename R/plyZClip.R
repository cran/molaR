#' Function which will clip a plyFile 
#'
#' Function which will find a localized maxima or minima and then clip
#' the plyFile above or below the selected maxima/minima along the Z-axis
#' 
#' @param plyFile an object of classes 'mesh3d' and/or 'shape3d'
#' @param invertZ True/False logical indicating whether to clip the surface above
#' or below the selected point. 
#' @param button logical either: 'right' (default), 'middle', or 'left' indicating which 
#' button on the mouse to use to interact with the plyFile
#' @param displayNew logical indicating whether or not to display a preview of 
#' the newly cropped ply surface. 
#' 
#' @details This function works through finding a local extema in a small 
#' highlighted area then using that as the Z-axis level to crop the surface. 
#' The function makes a few assumptions. That the occlusal plane is parallel to
#' the X-Y plane, and is normal to the positive Z-axis. Further, it is intended to 
#' be used with the occlusal plane oriented toward
#' the operator. The operator should then highlight a small area of the surface 
#' in which they believe the extrema of interest (i.e. the Z-level at which the
#' surface should be cropped) will be captured. This function is intended to crop
#' tooth files in such a way as to result in a tooth surface containing
#' only the area of the tooth crown above the lowest point of the occlusal basin.
#' This cropping procedure is consistent with the RFI measure presented by
#' Ungar and M'Kirera (2003).
#'
#' The logical invertZ allows users to account for surfaces which have been 
#' loaded upside-down. That is, the occlusal basin is normal to the negative
#' Z-axis. Set invertZ=True and the dental surface will be properly cropped.  
#' 
#' @import
#' rgl
#'
#' @export
#' plyZClip


plyZClip <- function(plyFile, invertZ = FALSE, button="right", displayNew= TRUE){
  if(button != "left" && button!="middle" && button!="right"){
    stop("Argument \'button\' should be one of \"left\", \"middle\", or \"right\"\n")
  }
  verts <- t(plyFile$vb[1:3,])
  open3d()
  points3d(verts)
  cat("Use", button, "mouse button to select points from point cloud in 3D window.\n")
  selecter <- select3d(button=button)
  keep <- selecter(verts)
  Keepers <- verts[keep,]
  rgl.pop()
  points3d(verts[keep,], size=3)
  if(invertZ==FALSE){
    lowZ <- min(Keepers[,3])
    lowPt <- which(Keepers[,3]==lowZ)
    lowPt <- Keepers[lowPt,]
    points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
    eliminate <- verts[,3]<lowZ
  }
  if(invertZ==TRUE){
    lowZ <- max(Keepers[,3])
    lowPt <- which(Keepers[,3]==lowZ)
    lowPt <- Keepers[lowPt,]
    points3d(x=lowPt[1], y=lowPt[2], z=lowPt[3], size=8)
    eliminate <- verts[,3]>lowZ
  }
  points3d(verts[eliminate,], col="red", size=3)
  points3d(verts[!keep,], col="gray")
  ActiveWin <- rgl.cur()[[1]]
  cat("User-selected points appear in BLACK\n")
  message("Points to be eliminated appear in RED.\n")
  cat("\nDo you want to clip mesh? [Y/N]\n")
  answer <- readline(prompt="")
  if(answer!="Y" && answer!="y" && answer!="N" && answer!="n"){
    message("Please enter \"Y\" or \"N\".\n")
    rgl.close()
    plyZClip(plyFile, invertZ = invertZ, button = button, displayNew = displayNew)
  }
  if(answer=="N" || answer=="n"){
    rgl.close()
    plyZClip(plyFile, invertZ = invertZ, button = button, displayNew = displayNew)
  }
  if(answer=="Y" || answer=="y"){
    if(rgl.cur() == ActiveWin){rgl.close()}
    NewVerts <- plyFile$vb[,!eliminate]
    plyFile$vb <- NewVerts
    NewNorms <- plyFile$normals[,!eliminate]
    plyFile$normals <- NewNorms
    BadVerts <- rev(which(eliminate))
    faces <- plyFile$it
    ChangeVerts <- as.vector(faces)
    for(i in 1:length(BadVerts)){
      bad <- BadVerts[i]
      ChangeVertList <- which(ChangeVerts>bad)
      ChangeVerts[ChangeVertList] <- ChangeVerts[ChangeVertList]-1
    }
    NewFaces <- matrix(ChangeVerts, nrow=3)
    VertList <- as.vector(faces)
    InFaces <- VertList %in% BadVerts
    NotIn <- which(InFaces)
    faces[NotIn] <- 0
    OrigSums <- colSums(plyFile$it)
    NewSums <- colSums(faces)
    LostFaces <- NewSums<OrigSums
    NewFaces <- NewFaces[,!LostFaces]
    plyFile$it <- NewFaces
  }
  if(displayNew==TRUE){
    open3d()
    shade3d(plyFile, col="gray")
    points3d(verts[BadVerts,], col="red", size=3)
  }
  return(plyFile)
}