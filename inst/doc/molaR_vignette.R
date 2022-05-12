## ----setup, message = FALSE---------------------------------------------------
library(molaR)
str(Tooth)

## ----vertices, message=FALSE--------------------------------------------------
Tooth$vb[,1:10]

## ----faces, message=FALSE-----------------------------------------------------
Tooth$it[,1:10]

## ----normals, message=FALSE---------------------------------------------------
Tooth$normals[,1:10]

## ----DNE_basic----------------------------------------------------------------
DNE1 <- DNE(Tooth)

## ----DNE_plot, eval=FALSE-----------------------------------------------------
#  DNE3d(DNE1, main='Vervet Tooth')

## ----DNE_plot1, fig.align='center', echo=FALSE--------------------------------
DNE3d(DNE1, main='Vervet Tooth')
rglwidget()

## ----DNE_setMax, eval=FALSE---------------------------------------------------
#  DNE3d(DNE1, setMax = 1.3, main='Vervet Tooth')

## ----str_DNE, DNE_object------------------------------------------------------
str(DNE1)
head(DNE1$Boundary_Values)
head(DNE1$Outliers)

## ----DNE3dDiscard, eval=FALSE-------------------------------------------------
#  DNE3dDiscard(DNE1, concaveCol='pink', main='Vervet Tooth')

## ----DNEpie, eval=FALSE-------------------------------------------------------
#  DNEpie(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth')
#  DNEpie(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth', type='DNE')

## ----DNEpie_plot, echo=FALSE, fig.show='hold', out.width='40%'----------------
DNEpie <- function(DNE_File, main='', type='area', convexCol='hotpink', concaveCol='deepskyblue') {
	DNE_object <- DNE_File
	if(type=='area') {
	slices <- c(DNE_object$Convex_Area, DNE_object$Concave_Area)
	}
	if(type=='DNE'){
	slices <- c(DNE_object$Convex_DNE, DNE_object$Concave_DNE)

	}
	if(type=='area'){
	names <- c('Convex Area', 'Concave Area')
	}
	if(type=='DNE'){
	names <- c('Convex DNE', 'Concave DNE')
	}
	ptc <- round(slices/sum(slices)*100)
	labels <- paste(names, paste(ptc, '%', sep=''), sep=' ')
	col1 <- col2rgb(convexCol)/255
	col2 <- col2rgb(concaveCol)/255
	pie(slices, clockwise=T, labels=labels, main=main, cex=0.85, radius=0.75, col=c(col=rgb(col1[1], col1[2], col1[3]), col=rgb(col2[1], col2[2], col2[3])))
	
}
DNEpie(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth')
DNEpie(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth', type='DNE')

## ----HillsDNE-----------------------------------------------------------------
Hills1 <- DNE(Hills)

## ----DNEpieHills, eval=FALSE--------------------------------------------------
#  DNEpie(Hills1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth Area')
#  DNEpie(Hills1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth DNE', type='DNE')

## ----DNEDensities, eval=FALSE-------------------------------------------------
#  DNEDensities(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth DNEs')
#  DNEDensities(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth \nFace Areas', type='area')

## ----DNEDensities_plot, echo=FALSE, fig.show='hold', out.width='40%'----------
DNEDensities <- function(DNE_File, main='', type='DNE', legendPos='topright', convexCol='hotpink', concaveCol='deepskyblue') {
DNE_object <- DNE_File
concaves <- which(DNE_object$Face_Values$Kappa_Values<DNE_object$Kappa)
if(type=='DNE') {
cavs <- log(DNE_object$Face_Values$Dirichlet_Energy_Densities[concaves])}
if(type=='area') {
cavs <- log(DNE_object$Face_Values$Face_Areas[concaves])	
}
dcavs <- density(cavs)
convexs <- which(DNE_object$Face_Values$Kappa_Values>DNE_object$Kappa)
if(type=='DNE') {
vexs <- log(DNE_object$Face_Values$Dirichlet_Energy_Densities[convexs])}
if(type=='area') {
vexs <- log(DNE_object$Face_Values$Face_Areas[convexs])	
}
dvexs <- density(vexs)
yuplim <- max(max(dvexs$y), max(dcavs$y))
xuplim <- max(max(dvexs$x), max(dcavs$x))

xlolim <- min(min(dvexs$x), min(dcavs$x))
if (type=='DNE') {
lab <- 'Log(DNE-Face Densities)'
}
if(type=='area') {
lab <- 'Log(Face Area)'	
}
plot(dvexs, ylim=c(0, yuplim), xlim=c(xlolim, xuplim), main=main, xlab=lab)
col1 <- col2rgb(convexCol)/255
col2 <- col2rgb(concaveCol)/255
polygon(dvexs, col=rgb(col1[1], col1[2], col1[3], 9/10))
polygon(dcavs, col=rgb(col2[1], col2[2], col2[3], 6/10))


if(type=='DNE') {
	legtex <- c('Convex DNEs', 'Concave DNEs')
}
if(type=='area') {
	legtex <- c('Convex Areas', 'Concave Areas')
}
legend(legendPos,legend=legtex, cex=0.5, fill=c(col=rgb(col1[1], col1[2], col1[3], 9/10), col=rgb(col2[1], col2[2], col2[3], 6/10)))

}

DNEDensities(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth DNEs')
DNEDensities(DNE1, convexCol='seashell', concaveCol='purple', main='Vervet Tooth \nFace Areas', type='area')

## ----DNEbar_list, eval=FALSE--------------------------------------------------
#  DNEs <- list()
#  
#  DNE1 <- DNE(Tooth)
#  DNE2 <- DNE(Hills)
#  
#  DNEs$Tooth <- DNE1
#  DNEs$Hills <- DNE2
#  
#  DNEbar(DNEs, convexCol='seashell', concaveCol='purple')

## ----DNEbar_plot_prep, echo=FALSE---------------------------------------------
DNEs <- list()

DNE1 <- DNE(Tooth)
DNE2 <- DNE(Hills)

DNEs$Tooth <- DNE1
DNEs$Hills <- DNE2

## ----DENbar_plot, echo=FALSE--------------------------------------------------
DNEbar(DNEs, convexCol='seashell', concaveCol='purple')

## ----RFI_basic----------------------------------------------------------------
RFI1 <- RFI(Tooth, alpha=0.08)

## ----Check2D, eval=FALSE------------------------------------------------------
#  Check2D(RFI1)

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
#  OPC3d(OPC1, scaleLegend=TRUE, main='Vervet Tooth')

## ----colors-------------------------------------------------------------------
colors <- c('firebrick', 'whitesmoke', 'deeppink', 'darkorchid', 'cornflowerblue', 'cyan', 'skyblue', 'turquoise')

## ----OPC_Custplot, eval=FALSE-------------------------------------------------
#  OPC3d(OPC1, binColors=colors, patchOutline=T, outlineColor='yellow')

## ----OPCbinareas_code, eval=FALSE---------------------------------------------
#  OPCbinareas(OPC1, main='Vervet Tooth')
#  OPCbinareas(OPC1, main='Vervet Tooth', type='pie')

## ----OPCbinareas_plots, echo=FALSE, fig.show='hold', out.width='40%'----------
OPCbinareas(OPC1, main='Vervet Tooth')
OPCbinareas(OPC1, main='Vervet Tooth', type='pie')

## ----Slope--------------------------------------------------------------------
Slope1 <- Slope(Tooth)

## ----Slope_plot, eval=FALSE---------------------------------------------------
#  Slope3d(Slope1)

## ----molaR_Batchex, eval=FALSE------------------------------------------------
#  molaR_Batch(pathName=~PathToYourFolder, filename=DNEBatch.csv, DNE=TRUE, RFI=FALSE, OPCr=FALSE, Slope=FALSE)

## ----molaR_Clean1, eval=FALSE-------------------------------------------------
#  Tooth.cleaned <- molaR_Clean(Tooth)

## ----molaR_Clean2, echo=FALSE-------------------------------------------------
Tooth.cleaned <- molaR_Clean(Tooth)

## ----PLYs_to_list, eval=FALSE-------------------------------------------------
#  files.teeth <- list.files(path='/Desktop/teeth', pattern='.ply')
#  
#  files <- list()
#  
#  l.files <- length(files.teeth)
#  
#  for(i in 1:l.files) {
#    file <- paste('~/Desktop/teeth/', files.teeth[i], sep='')
#    temp <- vcgPlyRead(file)
#    files[[i]] <- molaR_Clean(temp)
#    names(files)[i] <- files.teeth[i]
#  }

## ----DNEList_Object, eval=FALSE-----------------------------------------------
#  DNEs <- list()
#  for(i in 1:length(names(files))) {
#    DNEs[[i]] <- DNE(files[[i]])
#    names(DNEs)[i] <- names(files)[i]
#  }

## ----DNEList_Object1, eval=FALSE----------------------------------------------
#  DNEbar(DNEs)
#  
#  DNEpie(DNEs[[1]], main=as.character(names(DNEs)[1]))
#  
#  DNE3d(DNEs[[1]], main=as.character(names(DNEs)[1]))
#  
#  DNE3dDiscard(DNEs[[1]], main=as.character(names(DNEs)[1]))
#  
#  DNEDensities(DNEs[[1]], main=as.character(names(DNEs)[1]))

