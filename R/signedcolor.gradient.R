#' SignedColor gradient for concave faces
#'
#' set up signed color gradient
#' @param x vector of values to be converted to colors
#' @noRd

signedcolor.gradient <- function(x, colors=c('deeppink', 'magenta', 'purple', 'blue', 'skyblue', 'turquoise', 'aquamarine', 'springgreen','green', 'green', 'green', 'green', 'green','greenyellow','yellow','gold','orange','red', 'firebrick'), colsteps=1000) {
	return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(0,1,length.out=colsteps))])
}
