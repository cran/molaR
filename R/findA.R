#' Calculate the ideal alpha value of a surface
#'
#' A function that computes an ideal alpha parameter for use in the
#' \code{\link{RFI}} function.
#'
#' @param alpha Starting value for searching alpha values. See details.
#'
#' @details The function is designed to compute the best `alpha` parameter
#'   possible for calculation of 2D area in the \code{\link{RFI}} function. This
#'   is defined as the lowest value (to the nearest thousandth) returning no
#'   error or warning messages.
#'
#'   Run-time for the function depends on the number of alpha values it tests.
#'   Unfortunately there is no way to guess an appropriate alpha value a priori.
#'   Only experienced users should alter the default starting value of
#'   `0.064`, and only when they have strong reason to do so ahead of time.
#'   The default starting value was calculated as an average `alpha` in
#'   more than 250 dental surfaces.
#'
#'   After 100 unsuccessful attempts to find an appropriate `alpha`, the
#'   function will terminate.
#'
#' @importFrom alphahull ahull
#'
#' @noRd

findA <- function(pancake2, Alpha=0.064){
  Pancake <- pancake2
  CurAlpha <- Alpha
  HighAlpha <- CurAlpha+1
  LowAlpha <- 0
  for(i in 1:100){
    if(i==100){stop("Alpha search failed after 100 attempts!")}
    hull <- ahull(Pancake[, 1:2], alpha = CurAlpha)
    arcs <- hull$arcs
        if (length(arcs[, 7]) != length(unique(arcs[, 7]))) {
      LowAlpha <- CurAlpha
      CurAlpha <- CurAlpha+0.01
      if(CurAlpha>HighAlpha){CurAlpha <- CurAlpha-0.009}
      next
    }
    STedges <- arcs[, "end1"]
    EDedges <- arcs[, "end2"]
    if (length(which(STedges-EDedges==0))>0){
      LowAlpha <- CurAlpha
      CurAlpha <- CurAlpha+0.01
      if(CurAlpha>HighAlpha){CurAlpha <- CurAlpha-0.009}
      next
    }
    ACoords <- hull$xahull
    Or <- nrow(Pancake)
    center <- rep(Or, length(STedges))
    slices <- cbind(center, STedges, EDedges)
    EdgePts <- sort(unique(c(STedges, EDedges)))
    AlphaWarning <- FALSE
    for(k in 1:length(EdgePts)){
      PtofInt <- unname(ACoords[EdgePts[k],1:2])
      for(j in 1:nrow(slices)){
        Tri1 <- unname(ACoords[slices[j,1],1:2])
        Tri2 <- unname(ACoords[slices[j,2],1:2])
        Tri3 <- unname(ACoords[slices[j,3],1:2])
        BaryA <- (((Tri2[2]-Tri3[2])*(PtofInt[1]-Tri3[1]))+((Tri3[1]-Tri2[1])*(PtofInt[2]-Tri3[2])))/(((Tri2[2]-Tri3[2])*(Tri1[1]-Tri3[1]))+((Tri3[1]-Tri2[1])*(Tri1[2]-Tri3[2])))
        BaryB <- (((Tri3[2]-Tri1[2])*(PtofInt[1]-Tri3[1]))+((Tri1[1]-Tri3[1])*(PtofInt[2]-Tri3[2])))/(((Tri2[2]-Tri3[2])*(Tri1[1]-Tri3[1]))+((Tri3[1]-Tri2[1])*(Tri1[2]-Tri3[2])))
        BaryG <- 1-BaryA-BaryB
        if(BaryA>0 & BaryB>0 & BaryG>0){
          LowAlpha <- CurAlpha
          CurAlpha <- CurAlpha+0.001
          AlphaWarning <- TRUE
          break
        }
      }
      if(AlphaWarning==TRUE){break}
    }
    if(AlphaWarning==TRUE){next}
    if(LowAlpha==(CurAlpha-0.001)){break}
    HighAlpha <- CurAlpha
    CurAlpha <- CurAlpha-0.005
    if(CurAlpha<=LowAlpha){CurAlpha <- LowAlpha+0.001}
    if(CurAlpha<=0){CurAlpha <- 0.000001}
    if(CurAlpha==HighAlpha){break}
  }
  return(CurAlpha)
}
