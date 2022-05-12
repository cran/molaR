#' Plot advanced results of a DNE surface analysis 
#' 
#' @param DNE_File An object that stores the output of the `DNE()`
#' function
#' @param main User's title for the plot
#' @param type string determine which parameters to plot. Default='DNE'
#' also accepts 'area' to plot pie charts of the area. 
#' @param convexCol Color for the portion of the pie chart representing convex
#' contribution. Default='hotpink'. Accepts any color keyword. 
#' @param concaveCol Color for the portion of the pie chart representing concave
#' contribution. Default='deepskyblue'. Accepts any color keyword. 
#'
#' @details This function creates a pie chart of the total area or DNE of the surface 
#' originating from the concave or convex portions of the surface. The function 
#' defaults to plotting surface area, however, relative proportion of total DNE from the
#' concave and convex portions of the surface can be plotted by calling `type='DNE'`. 
#' Colors can be customized by altering the `convexCol` and `concaveCol` arguments. 
#' 
#' 
#' @export
#' DNEpie
#'
#' @examples
#' DNE_output <- DNE(Tooth)
#' DNEpie(DNE_output)

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
	dev.new()
	pie(slices, clockwise=T, labels=labels, main=main, cex=0.85, radius=0.75, col=c(col=rgb(col1[1], col1[2], col1[3]), col=rgb(col2[1], col2[2], col2[3])))
	
}










