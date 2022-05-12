#' Interpolate faces to vertices. 
#'
#' @param xface face on a mesh surface
#' @param ply PLY file to manipulate
#' 
#' interpolate_faces_to_vertices
#' @noRd


interpolate_faces_to_vertices <- function(xface,ply){
  
  vtf_list <- vcgVFadj(ply)
  xvert <- matrix(0,nrow=length(vtf_list),ncol=1)
  
  if(is.null(ply$Face_Areas)){
    Face_Areas <- vcgArea(ply, perface = TRUE)
    ply$Face_Areas <- Face_Areas$pertriangle
  }
  
  for (i in 1:length(vtf_list)) {
    vals <- xface[unlist(vtf_list[i])]
    # weighted average by face area
    weights <- ply$Face_Areas[unlist(vtf_list[i])]
    weights <- weights / sum(weights)
    xvert[i] <- sum(vals * weights)
  }
return(xvert)
}
