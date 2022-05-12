#' Plot advanced results of a DNE surface analysis 
#' 
#' a molaR plotting function
#'
#' @param DNE_File An object that stores the output of the DNE()
#' function
#' @param main User's title for plot. 
#' @param convexCol Color for the convex DNE total. Default='hotpink'
#' @param concaveCol Color for the concave DNE total. Default='deepskyblue'
#' @param type string to determine what parameters to plot. Default=both and 
#' both concave and convex DNE totals will be plotted in stacked bar plot. See details
#' @param legendPos string to determine location of the legend. Default='topright'
#' see details.
#' @param legendInset numeric value determining how far to inset the legend from plot
#' boarder. Default=0
#' @param las logical indicating orientation of the x-axis labels for each bar plot. Enter either 1 or 2. 
#' @param names.arg concatenated string of surface names for labels. If none supplied function will
#' pull names from the object itself. 
#' @param cex.names Font size for the bar labels. Default is 1. 
#'
#' @details This function creates a stacked barplot of DNE values. It colors them according
#' to curve orientation, which is defined by the `kappa` parameter in `DNE()` function. If multiple 
#' DNE objects are grouped together the barplot will return a set. When employed on a single
#' DNE object this will return a single stacked bar. 
#'
#' The argument `type` accepts either 'Concave' or 'Convex' to plot only concave or convex 
#' DNE totals respectively. Default=NA and results in both totals being plotted in stacked barplot.
#'
#' The argument `legendPos` is a string that determines the position of the legend. Default='topright'
#' but will accept any of the following keywords: 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top',
#' 'topright', 'right', or 'center'. 
#' 
#' 
#' @export
#' DNEbar
#'
#' @examples
#' DNEs <- list()
#' DNEs$Tooth <- DNE(Tooth)
#' DNEs$Hills <- DNE(Hills)
#' DNEbar(DNEs)

DNEbar <- function(DNE_File, main='', convexCol='hotpink', concaveCol='deepskyblue', type='both', legendPos='topright', legendInset=0, las=1, names.arg='', cex.names=1){
	if(type=='both') {
	if(inherits(DNE_File, 'DNE_Object')==T){
		mit <- as.matrix(c(DNE_File$Convex_DNE, DNE_File$Concave_DNE))
		labs <- names(DNE_File)
	}
	if(inherits(DNE_File, 'list')==T){
	count <- length(DNE_File)
	Vdne <- vector(mode='numeric', length=count)
	Cdne <- vector(mode='numeric', length=count)
	for (i in 1:count) {
		Vdne[i] <- DNE_File[[i]]$Convex_DNE
		Cdne[i] <- DNE_File[[i]]$Concave_DNE
		labs <- names(DNE_File)
	}
	mit <- rbind(Vdne, Cdne)
	mit <- as.matrix(mit)
	}
	if(length(names.arg)>1){
	labs <- names.arg	
	}
	barplot(mit, col=c(convexCol, concaveCol), main=main, xlab='DNE Surface', ylab='DNE Total', names.arg=c(labs), las=las, cex.names=cex.names)
	legend(legendPos, inset=legendInset, legend=c('Concave DNE', 'Convex DNE'), fill=c(concaveCol, convexCol), cex=0.5)
	}
	
	if(type!='both' && type!='Convex' && type!='Concave'){
			stop("Choose One Orientation to Plot, with type='Concave' or 'Convex' or leave as 'both'")
		} 	
	if(type=="Convex") {
			if(inherits(DNE_File, 'list')==T){
	count <- length(DNE_File)
	Vdne <- vector(mode='numeric', length=count)
	for (i in 1:count) {
		Vdne[i] <- DNE_File[[i]]$Convex_DNE
		labs <- names(DNE_File)
	}
	}
	if(length(names.arg)>1){
	labs <- names.arg	
	}
	barplot(Vdne, col=convexCol, main=main, xlab='DNE Surface', ylab='DNE Total', names.arg=c(labs), las=las, cex.names=cex.names)
	legend(legendPos, inset=legendInset, legend=c('Convex DNE'), fill=c(convexCol), cex=0.5)
	}
	if(type=='Concave') {
			if(inherits(DNE_File, 'list')==T){
	count <- length(DNE_File)
	Cdne <- vector(mode='numeric', length=count)
	for (i in 1:count) {
		Cdne[i] <- DNE_File[[i]]$Concave_DNE
		labs <- names(DNE_File)
	}
	}
	if(length(names.arg)>1){
	labs <- names.arg	
	}
	barplot(Cdne, col=concaveCol, main=main, xlab='DNE Surface', ylab='DNE Total', names.arg=c(labs), las=las, cex.names=cex.names)
	legend(legendPos, inset=legendInset, legend=c('Concave DNE'), fill=c(concaveCol), cex=0.5)		
}
}









