#' Conventional color gradient for faces
#'
#' set up signed color gradient
#' @param x vector of values to be converted to colors
#' @noRd

ccolor.gradient <- function(x, colors=c('blue', 'skyblue', 'turquoise', 'aquamarine', 'springgreen', 'green','greenyellow','yellow','gold','orange','red', 'firebrick'), colsteps=1000) {
	return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(0,1,length.out=colsteps))])
}