# Internal function for displaying cut meshes

clipDisplay <- function(clipOutput, plane, centroid, keepers = NA, focal = NA, kb){
  pl.Norm <- plane[1:3]
  Dist <- sum(pl.Norm*centroid, plane[4])/sqrt(sum(pl.Norm^2))
  Pl.pt <- centroid - Dist*pl.Norm/sqrt(sum(pl.Norm^2))
  oneMesh <- FALSE
  if(is.null(clipOutput$meshA) || is.null(clipOutput$meshB)){oneMesh <- TRUE}
  if(oneMesh){
    if(is.null(clipOutput$meshA)){shade3d(clipOutput$meshB, col = "white")}
    if(is.null(clipOutput$meshB)){shade3d(clipOutput$meshA, col = "white")}
    points3d(x = Pl.pt[1], y = Pl.pt[2], z = Pl.pt[3], col = "blue", alpha = 0.66)
    planes3d(a = plane[1], b = plane[2], c = plane[3], d = plane[4], col = "blue", alpha = 0.66)
  }
  else{
    if(!(length(keepers) == 1 && is.na(keepers))){points3d(keepers, size = 3)}
    if(!(length(focal) == 1 && is.na(focal))){points3d(x = focal[1], y = focal[2], z = focal[3], size = 8)}
    shade3d(clipOutput$meshA, col = "white")
    planes3d(a = plane[1], b = plane[2], c = plane[3], d = plane[4], col = "blue", alpha = 0.66)
    if(kb == FALSE){points3d(t(clipOutput$meshB$vb), col = "red", size = 3)}
    if(kb == TRUE){shade3d(clipOutput$meshB, col = "#404040")}
  }
}
