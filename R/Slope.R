#' Function to calculate the average slope of a surface
#'
#' A function that calculates the average slope over a tooth
#' or some other 3D surface
#'
#' @param plyFile An object of classes 'mesh3d' and 'shape3d' with calculated normals
#' @param Guess Logical indicating whether the function should 
#' 'guess' as to the 'up' direction for the surface and to remove negative
#' slopes from the calculation
#' 
#' @details This function requires a ply file. It will calculate the slope on each face
#' of the surface and will average the slope across the surface. This is functionally
#' equivalent to the slope calculation used by Ungar and M'Kirera "A solution to the worn 
#' tooth conundrum  in primate functional anatomy" PNAS (2003) 100(7):3874-3877
#'
#' In the case of applying this function to teeth (its intended purpose), the function expects a
#' surface with the occlusal plane normal to the Z-axis. 
#' 
#' The `Guess` parameter is a logical asking whether or not you want the function to both
#' guess as to the right side up of the surface, and to then discard all of the 'negative' slopes,
#' i.e. surfaces which are over-hangs, as is frequently found on the sidewalls of teeth. If 
#' `Guess` is not engaged the mean slope will include the negative values of the overhang
#' and will likely underestimate the average slope of the surface. 
#' 
#' Regardless of if the `Guess` parameter is engaged, the function will also return a vector
#' containing all of the face slope values ("Face_Slopes")
#'
#' @export
#' Slope
#'
#' @examples
#' Slope_output <- Slope(Tooth)
#' summary(Slope_output)

Slope <- function (plyFile, Guess = FALSE) 
{
  origPly <- plyFile
  plyFile <- Face_Normals(plyFile)
  Areas <- vcgArea(plyFile, perface = TRUE)
  Face_Areas <- Areas$pertriangle
  Norms <- plyFile$Face_Normals
  Face_Slopes <- round(acos(Norms[3, ]) * (180/pi))
  if (Guess == FALSE) {
    Slopes <- Face_Slopes * Face_Areas
    Mean_Slope <- sum(Slopes)/sum(Face_Areas)
    if (Mean_Slope > 0) {
      out <- list(Mean_Surface_Slope = abs(Mean_Slope), Face_Slopes = Face_Slopes,
                  Face_Normals = Norms, Face_Areas = Face_Areas, plyFile = origPly)
      cat("Average Surface Slope=", abs(Mean_Slope), 
          "\n")
    }
    if (Mean_Slope < 0) {
      out <- list(Mean_Surface_Slope = abs(Mean_Slope), 
                  `Surface Inversion` = "Surface is likely Inverted",
                  Face_Slopes = Face_Slopes, Face_Normals = Norms,
                  Face_Areas = Face_Areas, plyFile = origPly)
      cat("Average Surface Slope=", abs(Mean_Slope), 
          "\n")
      cat("Your surface is either upside down or inside out", 
          "\n")
      cat("Therefore mean slope is returned as an absolute value", 
          "\n")
      cat("The signs on the individual face slopes are unchanged")
    }
  }
  if (Guess == T) {
    if (sum(Face_Slopes) > 0) {
      Face_Slopes[which(Face_Slopes < 0)] <- 0
      Face_Areas[which(Face_Slopes < 0)] <- 0
      Mean_Slope <- sum(Face_Slopes * Face_Areas)/sum(Face_Areas)
      out <- list(Mean_Surface_Slope = Mean_Slope, Face_Slopes = Face_Slopes,
                  Face_Normals = Norms, Face_Areas = Face_Areas, plyFile = origPly)
      cat("Average Surface Slope=", Mean_Slope, "\n")
      cat("Function has Guessed that your surface is upright and", 
          "\n")
      cat("that you wanted all negative slopes (overhangs) removed from average.")
    }
    if (sum(Face_Slopes) < 0) {
      Face_Slopes <- Face_Slopes * -1
      Face_Slopes[which(Face_Slopes < 0)] <- 0
      Face_Areas[which(Face_Slopes < 0)] <- 0
      Mean_Slope <- sum(Face_Slopes * Face_Areas)/sum(Face_Areas)
      out <- list(Mean_Surface_Slope = Mean_Slope, 
                  `Surface Inversion` = "Surface is likely Inverted",
                  Face_Slopes = Face_Slopes, Face_Normals = Norms,
                  Face_Areas = Face_Areas, plyFile = origPly)
      cat("Average Surface Slope=", Mean_Slope, "\n")
      cat("Function has Guessed that your surface is upside down and", 
          "\n")
      cat("it has flipped it and removed the negative slopes (overhangs)")
    }
  }
  return(out)
}