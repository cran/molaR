#' Plot advanced results of a DNE surface analysis 
#' 
#' @param DNE_File An object that stores the output of the DNE
#' function
#' @param main User's title for plot. Default is blank
#' @param type string determining which density plots to make. Default
#' @param legendPos string to determine location of the legend. Default='topright'
#' see details.
#' is to plot DNE face densities. Alternatively can plot face areas with 'area'
#' @param convexCol Color for the convex density polygon, Default='hotpink'
#' @param concaveCol Color for the concave density polygon, Default='deepskyblue'
#'
#' @details This function creates a set of overlapping density plots of two potential types.
#' The user can plot overlapping density plots that sort the surface into concave and convex
#' portions for plotting. The function will default to plotting DNE density values, however
#' density of face surface areas sorted into concave and convex portions of the surface
#' can be plotted by calling `type='area'`. Colors can be customized by altering the 
#' `convexCol` and `concaveCol` arguments. 
#' 
#' @importFrom
#' stats density
#' 
#' @export
#' DNEDensities
#'
#' @examples
#' DNE_output <- DNE(Tooth)
#' DNEDensities(DNE_output)

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
dev.new()
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








