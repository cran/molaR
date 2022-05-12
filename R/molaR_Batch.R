#' Run molaR analyses on a batch of specimens
#'
#' A function that automates molaR analyses on multiple specimens. Several
#' different analyses can be performed on each surface, with specifications for
#' analysis parameters.
#'
#' @param pathName The path to the folder containing all ply surfaces to be
#' analyzed. Defaults to the working directory.
#' @param fileName Name for the output .csv file containing results and parameters
#' @param DNE Logical indicating whether or not to perform the DNE calculation
#' @param RFI Logical indicating whether or not to perform the RFI calculation
#' @param OPCr Logical indicating whether or not to perform the OPCr calculation
#' @param OPC Logical indicating whether or not to perform the OPC calculation
#' @param Slope Logical indicating whether or not to perform the Slope calculation
#' @param details Logical indicating whether or not to save additional output from
#' some of the topographic analyses
#' @param parameters Logical indicating whether or not to save the list of analysis
#' parameters used in the batch run
#' @param ... Additional arguments passed to the topographic analysis functions.
#' See Details.
#'
#' @details This function allows a user to set the analyses from molaR they want
#' to run on a batch of ply files. Output is saved to a csv file. By default, the
#' batch function will perform specified analyses on all ply files in the working
#' directory. A different folder can be specified with `pathName`. Output saves
#' as .csv to the folder that contains the analyzed ply files.
#' 
#' Any of the default arguments of the various topographic analysis functions can
#' be modified for the batch by specifying them when calling `molaR_Batch`, e.g.,
#' the DNE `kappa` value can be changed to 'X' by specifying `kappa = X`. Users
#' are **strongly** encouraged to review the documentation for \code{\link[=DNE]{DNE()}},
#' \code{\link[=RFI]{RFI()}}, \code{\link[=OPCr]{OPCr()}}, \code{\link[=OPC]{OPC()}}, 
#' and \code{\link[=Slope]{Slope()}} and to understand the effects of alterations before making 
#' changes. A recommended practice for analyzing RFI in a batch of specimens is to enable 
#' `findAlpha = TRUE` given that the ideal `alpha` value is likely to vary among different 
#' specimens. However, this will increase calculation time (see documentation for 
#' \code{\link[=RFI]{RFI}}).
#' 
#' By default, the batch output will retain some additional details of the analysis.
#' These include, in the case of DNE: convex and concave DNE values, convex and
#' concave surface areas; in the case of RFI: 3D and 2D surface areas and analysis
#' `alpha` values; in the case of OPCr: the surface OPC value calculated at each
#' rotation; and in the case of OPC: the patch count for each bin. These results
#' will be discarded and only the final result of each topographic analysis will
#' be retained if `details = FALSE`.
#' 
#' The function will save a list of all parameters used in all batch analyses
#' to the output .csv file, below the results. This can be suppressed with
#' `parameters = FALSE`, but is recommended as a check on how analyses were
#' performed when returning to results in the future. If the function is assigned
#' to an object in R, the parameters are not included in the resultant data.frame,
#' but will still be included in the .csv file by default.
#' 
#' Note that batch processing updates will not display by default if using RGui
#' for Windows. Disable Misc -> Buffered output (Ctrl+W) if you wish to view
#' batch processing progress in RGui for Windows.
#'
#' @importFrom
#' Rvcg vcgPlyRead
#'
#' @export
#' molaR_Batch

molaR_Batch <- function(pathName = getwd(), fileName = "molaR_Batch.csv",
                        DNE = TRUE, RFI = TRUE, OPCr = TRUE, OPC = FALSE,
                        Slope = TRUE, details = TRUE, parameters = TRUE, ...) 
{
  BatchArgs <- c(formals("molaR_Batch"), formals("DNE"), formals("RFI"),
                 formals("OPCr"), formals("OPC"), formals("Slope"))
  for (argname in names(BatchArgs)){
    if(BatchArgs[[argname]]==""){BatchArgs[argname] <- NULL}
  }
  Call <- c(as.list(environment()), list(...))
  Call[1:2] <- NULL
  for (argname in names(Call)){
    if (is.null(BatchArgs[[argname]])) {
      warning(gettextf("Attempt to set '%s' ignored", argname),
              immediate. = TRUE, domain = NA)
    }
  }
  ArgUpdate <- which(names(BatchArgs) %in% names(Call))
  for (argname in names(BatchArgs)[ArgUpdate]){
    BatchArgs[[argname]] <- Call[[argname]]
  }
  if((nchar(fileName)-regexpr(".csv", fileName, fixed=T)[[1]])!=3){
    fileName <- paste(sep="", fileName, ".csv")
  }
  if(fileName==".csv"){fileName <- "molaR_Batch.csv"}
  files <- dir(pathName, pattern = "*.ply")
  fileNum <- length(files)
  if (fileNum == 0) {stop("No PLY files in this directory")}
  DTA <- c("DNE", "RFI", "OPCr", "OPC", "Slope")
  if(all(BatchArgs[DTA] == FALSE)) { stop("No analyses were selected to run") }
  Output <- data.frame(File = vector(length = fileNum),
                       DNE = vector(length = fileNum),
                       Convex_DNE = vector(length = fileNum),
                       Concave_DNE = vector(length = fileNum),
                       Convex_Area = vector(length = fileNum),
                       Concave_Area = vector(length = fileNum),
                       RFI = vector(length = fileNum),
                       `3D_Area` = vector(length = fileNum),
                       `2D_Area` = vector(length = fileNum),
                       Alpha = vector(length = fileNum),
                       OPCR = vector(length = fileNum),
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
  if(BatchArgs$steps != round(BatchArgs$steps)){
    stop("Enter integer value for number of OPCr steps")
  }
  Rotations <- matrix(nrow = fileNum, ncol = BatchArgs$steps)
  OPCBins <- matrix(nrow = fileNum, ncol = 8)
  colnames(Rotations) <- paste(seq(0, by=BatchArgs$stepSize, length.out=ncol(Rotations)), "deg.")
  colnames(OPCBins) <- paste("Bin", seq(1, ncol(OPCBins)))
  Output <- cbind(Output, Rotations, OPC = vector(length = fileNum), OPCBins,
                  Slope = vector(length = fileNum))
  Output[] <- NA
  tryCatch(write.csv(Output, file = file.path(pathName, fileName)),
           error = function(err) {
             stop("Error writing output to specified directory")
           })
  file.remove(file.path(pathName, fileName))
  
  cat("\nBeginning batch processing...")
  
  for (i in 1:fileNum) {
    Output$File[i] <- files[i]
    invisible(capture.output({
      Spec <- try(molaR_Clean(vcgPlyRead(file.path(pathName, files[i])), verbose = FALSE))
    }))
    if (BatchArgs$DNE == TRUE) {
      invisible(capture.output({
        DNE_Spec <- try(DNE(plyFile = Spec,
                            outliers = BatchArgs$outliers,
                            kappa = BatchArgs$kappa,
                            BoundaryDiscard = BatchArgs$BoundaryDiscard, 
                            oex = BatchArgs$oex))
      }))
      if (is.character(DNE_Spec)) {Output$DNE[i] <- DNE_Spec}
      else {
        Output$DNE[i] <- DNE_Spec$Surface_DNE
        Output$Convex_DNE[i] <- DNE_Spec$Convex_DNE
        Output$Concave_DNE[i] <- DNE_Spec$Concave_DNE
        Output$Convex_Area[i] <- DNE_Spec$Convex_Area
        Output$Concave_Area[i] <- DNE_Spec$Concave_Area
      }
    }
    if (BatchArgs$RFI == TRUE) {
      invisible(capture.output({
        RFI_Spec <- try(suppressWarnings(RFI(plyFile = Spec,
                            alpha = BatchArgs$alpha,
                            findAlpha = BatchArgs$findAlpha)))
      }))
      if (is.character(RFI_Spec)) {Output$RFI[i] <- RFI_Spec}
      else {
        if (RFI_Spec$Alpha_Warning==TRUE) {
          Output$RFI[i] <- "ERROR: 2D Area over-estimated due to alpha tracing error. Re-run RFI() on this surface and Check2D() on output."
        }
        else {Output$RFI[i] <- RFI_Spec$Surface_RFI}
        Output$`3D_Area`[i] <- RFI_Spec$Three_D_Area
        Output$`2D_Area`[i] <- RFI_Spec$Two_D_Area
        Output$Alpha[i] <- RFI_Spec$Alpha
      }
    }
    if (BatchArgs$OPCr == TRUE) {
      invisible(capture.output({
        OPCr_Spec <- try(OPCr(plyFile = Spec,
                              steps = BatchArgs$steps,
                              stepSize = BatchArgs$stepSize,
                              minimum_faces = BatchArgs$minimum_faces,
                              minimum_area = BatchArgs$minimum_area))
      }))
      if (is.character(OPCr_Spec)) {Output$OPCR[i] <- OPCr_Spec}
      else {
        Output$OPCR[i] <- OPCr_Spec$OPCR
        Output[i,seq(12, 11+BatchArgs$steps)] <- OPCr_Spec$Each_Run[,2]
      }
    }
    if (BatchArgs$OPC == TRUE) {
      invisible(capture.output({
        OPC_Spec <- try(OPC(plyFile = Spec,
                            rotation = BatchArgs$rotation,
                            minimum_faces = BatchArgs$minimum_faces,
                            minimum_area = BatchArgs$minimum_area))
      }))
      if (is.character(OPC_Spec)) {Output$OPC[i] <- OPC_Spec}
      else {
        Output$OPC[i] <- OPC_Spec$Patch_Count$`total patches`
        PatchTotals <- OPC_Spec$Patch_Count$directions
        PatchTotals <- PatchTotals[order(as.numeric(dimnames(OPC_Spec$Patch_Count$directions)[[1]]))]
        Output[i,seq(ncol(Output)-8, ncol(Output)-1)] <- PatchTotals
      }
    }
    if (BatchArgs$Slope == TRUE) {
      invisible(capture.output({
        Slope_Spec <- try(Slope(plyFile = Spec,
                                Guess = BatchArgs$Guess))
      }))
      if (is.character(Slope_Spec)) {Output$Slope[i] <- Slope_Spec}
      else { Output$Slope[i] <- Slope_Spec$Mean_Surface_Slope}
    }
    cat("\nProcessed", i, "of", fileNum, "total ply files in directory...")
  }
  
  if (details == FALSE) {
    NoDeets <- c("File", DTA, "OPCR")
    Keepers <- which(colnames(Output) %in% NoDeets)
    Output <- Output[,-seq(ncol(Output))[-Keepers]]
  }
  NoData <- unname(which(apply(Output, 2, function(x) all(is.na(x)))))
  if(length(NoData)>0){Output <- Output[,-NoData]}
  Results <- Output
  cat("\n")
  
  if (parameters == TRUE) {
    if (ncol(Output) == 2) {
      Output[, 3] <- NA
      names(Output)[3] <- ""
    }
    Output[c(nrow(Output)+1, nrow(Output)+2),] <- NA
    Output[nrow(Output), 1:3] <- c("Analysis", "Argument", "Value")
    Analysis <- vector(); Argument <- vector(); Value <- vector()
    for(argname in DTA) {
      if(unname(BatchArgs[argname]) == TRUE){
        DTAParams <- which(names(BatchArgs) %in% names(formals(argname)))
        Analysis <- c(Analysis, rep(argname, length(DTAParams)))
        Argument <- c(Argument, names(BatchArgs)[DTAParams])
        for(val in 1:length(DTAParams)) {
          Value <- c(Value, unname(unlist(BatchArgs[DTAParams][val])))
        }
      }
    }
    Params <- matrix(c(Analysis, Argument, Value, rep(NA, (ncol(Output)-3)*length(Analysis))),
                     ncol = ncol(Output), byrow = FALSE)
    Dupes <- which(duplicated(Params[,2]))
    if(sum(BatchArgs$OPC, BatchArgs$OPCr)) {Params <- Params[-Dupes,]}
    colnames(Params) <- colnames(Output)
    Output <- rbind(Output, Params)
  }
  write.csv(Output, file = file.path(pathName, fileName), row.names = FALSE, na = "")
  cat("Results saved to directory.\n")
  print(Results)
}