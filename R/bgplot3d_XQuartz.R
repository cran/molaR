#' function for correcting new XQuartz issues
#'
#' crucial for plotting in Mac OS Yosemite and El Capitan
#'
#' @param expression expression calls from DNE3d RFI3d and OPC3d
#' bgplot3d_XQuartz() 


bgplot3d_XQuartz <- function(expression){ 
  width <- dev.size("px")[1]
  height <- dev.size("px")[2]
  if(width > 0 && height > 0){
  	filename <- tempfile(fileext=".png")
  	png(filename=filename, width=width, height=height)
  	value <- try(expression)
  	dev.off()
  	result <- bg3d(texture=filename, col="white")
  	unlink(filename)
  }
  else{
  	value <- NULL
  	result(bg3d(col="white"))
  }
  invisible(structure(result, value=value))
}