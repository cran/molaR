#' Calculate Curve Orientation for each Face on a Surface
#'
#' This function uses the orientation of vertex normals on each leg of each face to 
#' measure their relative degrees parallel with positive and negative orientation. 
#'
#' @param plyFile An object of class 'mesh3d' and 'shape3d' with calculated normals
#'
#' @importFrom
#' Rvcg vcgGetEdge
#'
#'
#' @noRd


CurveOrientation <- function(plyFile) {

tri <- plyFile$it
vert <- plyFile$vb[1:3,]
nv <- plyFile$normals[1:3,]

kappaface =matrix(0, nrow = 3, ncol=length(tri[1,]))
idx = matrix(c(2,1,3,1,1,2),nrow=2,ncol=3)

# compute curvature along each edge of every triangle
for (i in 1:3){    
    j = idx[1,i]; k = idx[2,i];
    
    nvdiff = nv[,tri[j,]] -  nv[,tri[k,]];
    vdiff = vert[,tri[j,]] - vert[,tri[k,]];

kappaface[j,] = colSums(nvdiff*vdiff)/colSums(vdiff^2);
}
# find the mean curvature on the face
kappaface = (1/3)*colSums(kappaface);
plyFile$kappaface <- kappaface

return(plyFile)

}