## ----setup, message = FALSE---------------------------------------------------
library(molaR)
str(Tooth)

## ----DNE_basic----------------------------------------------------------------
DNE1 <- DNE(Tooth)

## ----DNE_plot, eval=FALSE-----------------------------------------------------
#  DNE3d(DNE1)

## ----DNE_plot1, fig.align='center', echo=FALSE--------------------------------
DNE3d(DNE1)
rglwidget()

## ----DNE_setRange, eval=FALSE-------------------------------------------------
#  DNE3d(DNE1, setRange = c(0, 1.3))

## ----DNE_setRange_plot, echo=FALSE--------------------------------------------
DNE3d(DNE1, setRange =  c(0, 1.3))
rglwidget()

## ----str_DNE, DNE_object------------------------------------------------------
str(DNE1)
head(DNE1$Boundary_Values)
head(DNE1$Outliers)

## ----RFI_basic----------------------------------------------------------------
RFI1 <- RFI(Tooth, alpha=0.08)

## ----Check2D, eval=FALSE------------------------------------------------------
#  Check2D(RFI1)

## ----Check2D_plot, echo=FALSE-------------------------------------------------
Check2D(RFI1)
rglwidget()

## ----RFI_plot, eval=FALSE-----------------------------------------------------
#  RFI3d(RFI1)

## ----OPC_basic----------------------------------------------------------------
OPC1 <- OPC(Tooth)

## ----OPC_patch_count----------------------------------------------------------
OPC2 <- OPC(Tooth, minimum_faces = 20)
OPC3 <- OPC(Tooth, minimum_area = 0.01)

## ----OPCr, eval=FALSE---------------------------------------------------------
#  OPCr_Example1 <- OPCr(Tooth)
#  OPCr_Example2 <- OPCr(Tooth, Steps = 5, stepSize = 9, minimum_faces = 2) #minimum_faces & minimum_area are passed to each iteration of OPC

## ----echo=FALSE---------------------------------------------------------------
OPCr_Example1
OPCr_Example2

## ----OPCr_structure-----------------------------------------------------------
OPCr_Example1$Each_Run
OPCr_Example2$Each_Run

## ----OPC_plot, eval=FALSE-----------------------------------------------------
#  OPC3d(OPC1)

## ----OPC_plot1, echo=FALSE----------------------------------------------------
OPC3d(OPC1)
rglwidget()

## ----colors-------------------------------------------------------------------
colors <- c('firebrick', 'whitesmoke', 'deeppink', 'darkorchid', 'cornflowerblue', 'cyan', 'skyblue', 'turquoise')

## ----OPC_Custplot, eval=FALSE-------------------------------------------------
#  OPC3d(OPC1, binColors=colors, patchOutline=T, outlineColor='yellow')

## ----Slope--------------------------------------------------------------------
Slope1 <- Slope(Tooth)

## ----Slope_plot, eval=FALSE---------------------------------------------------
#  Slope3d(Slope1)

## ----Slope_plot1, echo=FALSE--------------------------------------------------
Slope3d(Slope1)
rglwidget()

