#' Function to find Face Normals
#'
#' This function re-computes the face normals
#' @param plyFile a stanford PLY file  
#' @noRd

Face_Normals <- function(plyFile){
Faces <- plyFile$it

plyFile$Face_Normals <- matrix(0, nrow=length(Faces[1,]), ncol=3)
FNormals <- plyFile$Face_Normals
verts <- plyFile$vb
verts <- verts[1:3,]

Vec1 <- verts[,Faces[2,]] - verts[,Faces[1,]]; 
Vec2 <- verts[,Faces[3,]] - verts[,Faces[1,]];

FNormals <- cross(Vec1,Vec2)
FNormals <- FNormals/repmat(sqrt(colSums(FNormals^2)),3,1)

plyFile$Face_Normals <- FNormals
return(plyFile)
}

