#' function for making a list of faces on each vertex
#'
#' crucial function for getting a list of faces which will gather the faces per vertex.
#' @param plyFile a stanford PLY file  
#'
#' @noRd

vertex_to_face_list <- function(plyFile){
	tri <- plyFile$it; N = size(tri,2)
I = Reshape(repmat(1:N,3,1),3*N,1) # these are the face indices 
V = Reshape(tri,3*N,1) # these are the vertex indices

# Combine and sort by vertex index
VT = cbind(V,I);
e1 <- sort.int(VT[,1],index.return=TRUE) # sort so that vertex indices are in ascending order
VT <- VT[e1$ix,]

# Find indices of first occurence of each unique vertex index
firstoccurence <- order(VT[,1])[!duplicated(sort(VT[,1]))]
firstoccurence <- sort(firstoccurence) # make sure they're ordered

firstoccurence[length(firstoccurence)+1] = length(VT[,2])+1
f <- function(I) VT[firstoccurence[I]:(firstoccurence[I+1]-1),2];
numvertices = size(plyFile$vb,2)
v2facelist <- arrayfun(f,1:numvertices);
return(v2facelist)

}
