#' Run a batch of molaR analyses
#'
#' A function which automates molaR analyses. User simply sets up the functions they
#' want run and can leave the computer to do the rest. 
#'
#' @param pathname The path to the file containing all the PLY surfaces to be
#' analyzed. Defaults to the working directory
#' @param filename Name for the output csv file.
#' @param DNE logical indicating whether or not to perform DNE calculation Defaults
#' to true
#' @param RFI logical indicating whether or not to perform RFI calculation Defaults to
#' true
#' @param OPCr logical indicating whether or not to perform OPCr calculation
#' Defaults to true
#' @param OPC logical indicating whether or not to perform OPC calculation Defaults
#' to false
#' @param Slope logical indicating whether or not to perform Slope calculation, Defaults
#' to true
#' @param Details logical indicating whether or not to save the details of the RFI and
#' OPCr calculations
#' @param DNE_outliers the percentile at which outliers will be excluded is passed to
#' the DNE function, defaults to 0.1
#' @param DNE_BoundaryDiscard is a logical indicating how
#' to handle the exclusion of the faces on the edge of the surface, defaults to excluding
#' faces which have a leg on the boundary. 
#' @param RFI_alpha the size of the alpha passed to RFI
#' function, defaults to 0.01
#' @param OPCr_steps the number of steps the OPCr function should take, is passed to
#' the OPCr function. Defaults to 8
#' @param OPCr_stepSize the size of each rotation. Passed to the OPCr function.
#' Defaults to 5.626 degrees
#' @param OPCr_minimum_faces sets the lower boundary for number of faces a patch
#' must have for inclusion in total count. Defaults to 3 or more.
#' @param OPCr_minimum_area sets the lower boundary for percentage of the surface
#' area a patch must make up for inclusion in the total patch count. Cannot be used with
#' minimum_faces on. Defaults to zero
#' @param OPC_rotation amount of rotation to apply during OPC calculation. Defaults
#' to zero
#' @param OPC_minimum_faces minimum number of faces a patch must contain to be
#' counted in the OPC function. Defaults to 3.
#' @param OPC_minimum_area minimum percentage of the surface area a patch must
#' make up to be counted in the OPC function. Defaults to off
#' @param Slope_Guess logical indicating whether or not to Guess as to the orientation
#' of the surface during the Slope calculation (see Slope function for details)
#' @param Parameters defaults to off. When engaged a list of all the parameters used
#' during molaR analysis will be appended to the output file. 
#'
#' @details This function allows a user to set the analyses from molaR they want to run,
#' along with the specific parameters for each function and have a whole batch of PLY
#' files analyzed and saved to a csv file. Function will perform analyses on all PLY files
#' in the working directory or user can specify a pathname to a folder containing PLY files.
#' Output saves to the folder that contains the analyzed PLY files.
#' 
#' This function will accept a vector of parameters for any of the function arguments if the
#' user wishes to vary the settings over the course of the batch run. It is recommended that
#' when making use of this feature the Parameters argument is set to TRUE for a record of
#' how analyses were performed.
#' 
#' Note that batch processing updates will not display by default if using RGui for Windows. Be
#' sure to disable Misc -> Buffered output (Ctrl+W) if you wish to view batch processing
#' progress in RGui for Windows.
#'
#' @importFrom
#' Rvcg vcgPlyRead
#'
#' @export
#' molaR_Batch


molaR_Batch <- function (pathname = getwd(), filename = "molaR_Batch.csv", DNE = TRUE, 
                         RFI = TRUE, OPCr = TRUE, OPC = FALSE, Slope= TRUE, Details = FALSE, DNE_outliers = 0.1, 
                         DNE_BoundaryDiscard = "Leg", RFI_alpha = 0.01, OPCr_steps = 8, 
                         OPCr_stepSize = 5.625, OPCr_minimum_faces = 3, OPCr_minimum_area = 0, 
                         OPC_rotation = 0, OPC_minimum_faces = 3, OPC_minimum_area = 0, Slope_Guess = FALSE,
                         Parameters = FALSE) 
{
  if((nchar(filename)-regexpr(".csv", filename, fixed=T)[[1]])!=3){
    filename <- paste(sep="", filename, ".csv")
  }
  fileNames <- dir(pathname, pattern = "*.ply")
  if (length(fileNames) == 0) {
    stop("No PLY files in this directory")
  }
  fileNum <- length(fileNames)
  if(fileNum%%length(DNE)>0){warning(paste("Length of 'DNE' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  DNE <- rep(DNE, fileNum)[1:fileNum]
  if(fileNum%%length(RFI)>0){warning(paste("Length of 'RFI' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  RFI <- rep(RFI, fileNum)[1:fileNum]
  if(fileNum%%length(OPCr)>0){warning(paste("Length of 'OPCr' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPCr <- rep(OPCr, fileNum)[1:fileNum]
  if(fileNum%%length(OPC)>0){warning(paste("Length of 'OPC' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPC <- rep(OPC, fileNum)[1:fileNum]
  if(fileNum%%length(Slope)>0){warning(paste("Length of 'Slope' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  Slope <- rep(Slope, fileNum)[1:fileNum]
  if(fileNum%%length(DNE_outliers)>0){warning(paste("Length of 'DNE_outliers' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  DNE_outliers <- rep(DNE_outliers, fileNum)[1:fileNum]
  if(fileNum%%length(DNE_BoundaryDiscard)>0){warning(paste("Length of 'DNE_BoundaryDiscard' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  DNE_BoundaryDiscard <- rep(DNE_BoundaryDiscard, fileNum)[1:fileNum]
  if(fileNum%%length(RFI_alpha)>0){warning(paste("Length of 'RFI_alpha' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  RFI_alpha <- rep(RFI_alpha, fileNum)[1:fileNum]
  if(fileNum%%length(OPCr_steps)>0){warning(paste("Length of 'OPCr_steps' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPCr_steps <- rep(OPCr_steps, fileNum)[1:fileNum]
  if(fileNum%%length(OPCr_stepSize)>0){warning(paste("Length of 'OPCr_stepSize' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPCr_stepSize <- rep(OPCr_stepSize, fileNum)[1:fileNum]
  if(fileNum%%length(OPCr_minimum_area)>0){warning(paste("Length of 'OPCr_minimum_area' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPCr_minimum_area <- rep(OPCr_minimum_area, fileNum)[1:fileNum]
  if(fileNum%%length(OPCr_minimum_faces)>0){warning(paste("Length of 'OPCr_minimum_faces' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPCr_minimum_faces <- rep(OPCr_minimum_faces, fileNum)[1:fileNum]
  if(fileNum%%length(OPC_rotation)>0){warning(paste("Length of 'OPC_rotation' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPC_rotation <- rep(OPC_rotation, fileNum)[1:fileNum]
  if(fileNum%%length(OPC_minimum_area)>0){warning(paste("Length of 'OPC_minimum_area' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPC_minimum_area <- rep(OPC_minimum_area, fileNum)[1:fileNum]
  if(fileNum%%length(OPC_minimum_faces)>0){warning(paste("Length of 'OPC_minimum_faces' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  OPC_minimum_faces <- rep(OPC_minimum_faces, fileNum)[1:fileNum]
  if(fileNum%%length(Slope_Guess)>0){warning(paste("Length of 'Slope_Guess' is not a factor of number of surfaces in", pathname, "\n  Errors may occur in processing."), immediate. = TRUE)}
  Slope_Guess <- rep(Slope_Guess, fileNum)[1:fileNum]
  cat("\nBeginning batch processing...")
  allFalse <- vector()
  for(i in 1:fileNum){
    if (DNE[i] == FALSE && RFI[i] == FALSE && OPCr[i] == FALSE && OPC[i] == FALSE && Slope[i] == FALSE) {
      allFalse <- c(allFalse, i)
    }
  }
  if(length(allFalse)>0){
    allFalse <- paste(as.character(allFalse), collapse = ", ")
    stop(paste("No metrics were selected to run on surface number(s):", allFalse))
  }
  DNE_Output <- vector("numeric")
  RFI_Output <- vector("numeric")
  OPCr_Output <- vector("numeric")
  OPC_Output <- vector("numeric")
  Slope_Output <- vector("numeric")
  if (Details == TRUE) {
    ThreeD_Area <- vector("numeric")
    TwoD_Area <- vector("numeric")
    CorrectlyOriented <- vector("logical")
    if(length(unique(OPCr_stepSize))==1 && length(unique(OPCr_steps))==1){
      RotDegrees <- seq(0, (OPCr_steps[1] * OPCr_stepSize[1]) - OPCr_stepSize[1], OPCr_stepSize[1])
      Rotations <- matrix(nrow = length(fileNames), ncol = length(RotDegrees))
      colnames(Rotations) <- paste(as.character(RotDegrees), "deg.")
    }
    else{
      AllRots <- vector()
      for(i in 1:fileNum){
        RotDegrees <- seq(0, (OPCr_steps[i] * OPCr_stepSize[i]) - OPCr_stepSize[i], OPCr_stepSize[i])
        AllRots <- c(AllRots, RotDegrees)
      }
      AllRots <- sort(unique(AllRots))
      Rotations <- matrix(nrow = length(fileNames), ncol = length(AllRots))
      colnames(Rotations) <- paste(as.character(AllRots), "deg.")
    }
  }
  for (i in 1:fileNum) {
    invisible(capture.output(Specimen <- molaR_Clean(vcgPlyRead(file.path(pathname, 
                                                                          fileNames[i])))))
    if (DNE[i] == TRUE) {
      invisible(capture.output({
        DNE_Specimen <- try(DNE(Specimen, outliers = DNE_outliers[i], 
                                BoundaryDiscard = DNE_BoundaryDiscard[i]))
      }))
      if (is.character(DNE_Specimen)) {
        DNE_Output <- c(DNE_Output, DNE_Specimen)
      }
      else {
        DNE_Result <- DNE_Specimen$Surface_DNE
        DNE_Output <- c(DNE_Output, DNE_Result)
      }
    }
    if (DNE[i] == FALSE) {DNE_Output <- c(DNE_Output, NA)}
    if (RFI[i] == TRUE) {
      invisible(capture.output({
        RFI_Specimen <- try(RFI(Specimen, alpha = RFI_alpha[i]))
      }))
      if (is.character(RFI_Specimen)) {
        RFI_Output <- c(RFI_Output, RFI_Specimen)
        if (Details == TRUE) {
          ThreeD_Result <- NA
          TwoD_Result <- NA
          ThreeD_Area <- c(ThreeD_Area, ThreeD_Result)
          TwoD_Area <- c(TwoD_Area, TwoD_Result)
        }
      }
      else {
        if (RFI_Specimen$Alpha_Warning==TRUE){
          if (Details == TRUE) {
            ThreeD_Result <- RFI_Specimen$Three_D_Area
            TwoD_Result <- RFI_Specimen$Two_D_Area
            ThreeD_Area <- c(ThreeD_Area, ThreeD_Result)
            TwoD_Area <- c(TwoD_Area, TwoD_Result)
          }
          RFI_Specimen <- "ERROR: 2D Area over-estimated due to alpha tracing error. Re-run RFI() on this surface and Check2D() on output."
          RFI_Output <- c(RFI_Output, RFI_Specimen)
        }
        else {
          RFI_Result <- RFI_Specimen$Surface_RFI
          RFI_Output <- c(RFI_Output, RFI_Result)
          if (Details == TRUE) {
            ThreeD_Result <- RFI_Specimen$Three_D_Area
            TwoD_Result <- RFI_Specimen$Two_D_Area
            ThreeD_Area <- c(ThreeD_Area, ThreeD_Result)
            TwoD_Area <- c(TwoD_Area, TwoD_Result)
          }
        }
      }
    }
    if (RFI[i] == FALSE) {
      RFI_Output <- c(RFI_Output, NA)
      if (Details == TRUE) {
        ThreeD_Result <- NA
        TwoD_Result <- NA
        ThreeD_Area <- c(ThreeD_Area, ThreeD_Result)
        TwoD_Area <- c(TwoD_Area, TwoD_Result)
      }
    }
    if (OPCr[i] == TRUE) {
      if (OPCr_steps[i] != round(OPCr_steps[i])) {
        stop("Please enter integer value for number of OPCr steps")
      }
      invisible(capture.output({
        OPCr_Specimen <- try(OPCr(Specimen, Steps = OPCr_steps[i], 
                                  stepSize = OPCr_stepSize[i], minimum_faces = OPCr_minimum_faces[i], 
                                  minimum_area = OPCr_minimum_area[i]))
      }))
      if (is.character(OPCr_Specimen)) {
        OPCr_Output <- c(OPCr_Output, OPCr_Specimen)
      }
      else {
        OPCr_Result <- OPCr_Specimen$OPCR
        OPCr_Output <- c(OPCr_Output, OPCr_Result)
        if (Details == TRUE) {
          if(length(unique(OPCr_stepSize))==1 && length(unique(OPCr_steps))==1){
            Rotations[i,] <- OPCr_Specimen$Each_Run[,2]
          }
          else{
            OPCr_Rots <- OPCr_Specimen$Each_Run[,1]
            ColIndices <- vector()
            for(k in 1:length(OPCr_Rots)){
              CurIndex <- which(mapply(function(x, y) {isTRUE(all.equal(x, y))}, AllRots, OPCr_Rots[k]))
              ColIndices <- c(ColIndices, CurIndex)
            }
            Rotations[i, ColIndices] <- OPCr_Specimen$Each_Run[,2]
          }
        }
      }
    }
    if (OPCr[i] == FALSE) {
      OPCr_Output <- c(OPCr_Output, NA)
    }
    if (OPC[i] == TRUE) {
      invisible(capture.output({
        OPC_Specimen <- try(OPC(Specimen, rotation = OPC_rotation[i], 
                                minimum_faces = OPC_minimum_faces[i], minimum_area = OPC_minimum_area[i]))
      }))
      if (is.character(OPC_Specimen)) {
        OPC_Output <- c(OPC_Output, OPC_Specimen)
      }
      else{
        OPC_Result <- OPC_Specimen$Patch_Count$`total patches`
        OPC_Output <- c(OPC_Output, OPC_Result)
      }
    }
    if (OPC[i] == FALSE) {OPC_Output <- c(OPC_Output, NA)}
    if (Slope[i] == TRUE) {
      invisible(capture.output({
        Slope_Specimen <- try(Slope(Specimen, Guess = Slope_Guess[i]))
      }))
      if (is.character(Slope_Specimen)) {
        Slope_Output <- c(Slope_Output, Slope_Specimen)
        if (Details==TRUE) {
          CorrectlyOriented <- c(CorrectlyOriented, NA)
        }
      }
      else{
        Slope_Result <- Slope_Specimen$Mean_Surface_Slope
        Slope_Output <- c(Slope_Output, Slope_Result)
        if (Details == TRUE) {
          if (Slope_Specimen[2]=='Surface is likely Inverted'){
            Oriented_Specimen <- FALSE
          }
          else{Oriented_Specimen <- TRUE}
          CorrectlyOriented <- c(CorrectlyOriented, Oriented_Specimen)
        }
      }
    }
    if (Slope[i] == FALSE) {
      Slope_Output <- c(Slope_Output, NA)
      if (Details==TRUE) {
        CorrectlyOriented <- c(CorrectlyOriented, NA)
      }
    }
    cat("\nProcessed", i, "of", fileNum, "total PLY files in directory")
  }
  Output <- data.frame(Files = fileNames)
  if (sum(DNE) > 0) {
    Output <- cbind(Output, DNE = DNE_Output)
  }
  if (sum(RFI) > 0) {
    Output <- cbind(Output, RFI = RFI_Output)
    if (Details == TRUE) {
      Output <- cbind(Output, `3D_Area` = ThreeD_Area, 
                      `2D_Area` = TwoD_Area)
    }
  }
  if (sum(OPCr) > 0) {
    Output <- cbind(Output, OPCR = OPCr_Output)
    if (Details == TRUE) {
      Output <- cbind(Output, Rotations)
    }
  }
  if (sum(OPC) > 0) {
    Output <- cbind(Output, OPC = OPC_Output)
  }
  if (sum(Slope) > 0) {
    Output <- cbind(Output, Slope = Slope_Output)
    if (Details ==TRUE) {
      Output <- cbind(Output, CorrectlyOriented)
    }
  }
  cat("\n")
  print(Output)
  if (Parameters == TRUE){
    if(sum(DNE)>0){
      Output <- cbind(Output, DNE_Outliers=DNE_outliers, DNE_BoundDiscard=DNE_BoundaryDiscard)
    }
    if(sum(RFI)>0){
      Output <- cbind(Output, RFI_Alpha=RFI_alpha)
    }
    if(sum(OPCr)>0){
      Output <- cbind(Output, OPCR_Steps=OPCr_steps, OPCR_StepSize=OPCr_stepSize,
                      OPCR_MinFaces=OPCr_minimum_faces, OPCR_MinArea=OPCr_minimum_area)
    }
    if(sum(OPC)>0){
      Output <- cbind(Output, OPC_Rotation=OPC_rotation, OPC_MinFaces=OPC_minimum_faces,
                      OPC_MinArea = OPC_minimum_area)
    }
    if(sum(Slope)>0){
      Output <- cbind(Output, Slope_Guess=Slope_Guess)
    }
  }
  write.csv(Output, file = file.path(pathname, filename), row.names = FALSE)
  alarm()
  cat("Results saved to directory.\n")
}