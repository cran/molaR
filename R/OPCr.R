#' Calculate average orientation patch count after several rotations
#'
#' A function that calls OPC iteratively after rotating mesh a selected
#' number of degrees around the Z-axis following Evans and Jernvall
#' (2009) Patterns and constraints in carnivoran and rodent dental
#' complexity and tooth size. J Vert Paleo 29:24A 
#'
#' @param plyFile An object of classes 'mesh3d' and 'shape3d' with
#' calculated normals
#' @param steps Number of iterations to run the `OPC()` function on
#' the mesh
#' @param stepSize Amount of rotation (in degrees) about the Z-axis
#' to adjust mesh surface by between each iteration of `OPC()`
#' @param minimum_faces Argument to pass to the `OPC()` function
#' @param minimum_area Argument to pass to the `OPC()` function
#'
#' @details The function requires a mesh object created by reading in a ply file utilizing
#' either the, \code{\link[Rvcg]{vcgPlyRead}} function.
#'
#' Default number of steps is 8, with a stepSize of 5.625 degrees, following the
#' original published definition of OPCR.
#'
#' See the details for the \code{\link{OPC}} function for more information about
#' preparing mesh surfaces and the effects of `minimum_faces` and `minimum_area`.
#'
#' @import
#' utils
#'
#' @export
#' OPCr


OPCr <- function(plyFile, steps=8, stepSize=5.625, minimum_faces=3, minimum_area=0){
  
  options(expressions=50000)
  Output <- matrix(nrow=steps, ncol=2)
  colnames(Output) <- c("Degrees_Rotated", "Calculated_OPC")
  j <- 0
  
  ## Run OPC function for the specified number of rotations, rotating by the specified stepSize 
  ## each time: ##
  for(i in 1:steps){
    Dummy <- NULL
    Run <- NULL
    
    ## Runs OPC, but suppresses output, taking only the total patch count ##
    Dummy <- capture.output(Run <- OPC(plyFile=plyFile, rotation=j, minimum_faces=minimum_faces,
                                       minimum_area=minimum_area))
    Values <- unlist(strsplit(grep(c(" "), Dummy, value=TRUE), " "))
    Output[i,2] <- as.numeric(Values[6])
    Output[i,1] <- j
    j <- j + stepSize
    gc(verbose=FALSE)
  }
  
  ## Average of the runs is OPCR value ##
  Result <- round(mean(Output[,2]), 2)
  
  out <- list("OPCR"=Result, "Each_Run"=Output)
  cat("Average patch count after", steps, "OPC calculations at", stepSize, "degrees \nrotated between each calculation =", Result, "\n")
  return(out)
}