#' Important function for re-doing the vertex normals for the DNE calculation. 
#'
#' The geomorph import function does not generate the correct vertex normals.
#' @param plyFile a stanford PLY file  
#'
#' @noRd


Equal_Vertex_Normals <- function(plyFile) {
VertFace <- vcgVFadj(plyFile)
FaceVert <- plyFile$it
v <- plyFile$vb
rawNorms <- plyFile$normals*0
rownames(rawNorms) <- c('x', 'y', 'z', 'length')
FaceNorms <- Face_Normals(plyFile)
FaceNorms <- FaceNorms$Face_Normals
for (i in 1:length(VertFace)) {
	if (length(VertFace[[i]])==1) {
		Myles <- FaceNorms[,VertFace[[i]]]
	  Myles <- c(Myles/sqrt(sum(Myles^2)), 1)
	}
	else {
	Myles <- rowMeans(FaceNorms[,VertFace[[i]]])
	Myles <- c(Myles/sqrt(sum(Myles^2)), 1)
	rawNorms[,i] <- Myles
	}
}
plyFile$normals <- rawNorms
return(plyFile)
}