#' Visualize surface area distribution into separate OPC orientation bins. 
#'
#'
#'  This function will make either a bar plot or pie 
#' chart showing the surface area assigned to each
#' OPC orientation bin. 
#'
#'
#' @param OPC_File An object that stores the output
#' of an OPC analysis using `OPC()`. 
#' @param main Title for plot. 
#' @param binColors Allows the user to define the fill colors for
#' each directional bin. see details
#' @param type String argument to determine type of plot, either
#' bar' or 'pie'. Default is set to 'bar' 
#'
#' @details This function will create either bar or pie chats visualising
#' the distribution of surface area into each of the OPC orientation 
#' bins. Colors can be customized but are meant to match the 
#' default settings in the `OPC3d()` function. 
#'
#'
#' @export
#' OPCbinareas
#'
#' @examples
#' OPC_Object <- OPC(Tooth)
#' OPCbinareas(OPC_Object) 


OPCbinareas <- function(OPC_File, main='', binColors=hsv(h=(seq(10, 290, 40)/360), s=0.9, v=0.85), type='bar') {
	PD <- OPC_File$Patch_Details
	
	bins <- numeric(length=length(PD))
	
	param <- OPC_File$Parameters
	criteria <- which(param!=0)
	if(length(criteria)==1){
		criteria <- param[[2]]
		for (i in 1:length(PD)){
			legit <- which(PD[[i]][,1]>=criteria)
			bins[i] <- sum(PD[[i]][legit,2])
			names(bins) <- names(PD)
		}
	}
	if(length(criteria)==2){
		criteria <- param[[3]]
		for (i in 1:length(PD)){
			legit <- which(PD[[i]][,2]>=criteria)
			bins[i] <- sum(PD[[i]][legit,2])
			names(bins) <- names(PD)
		}
	}
	
	bins2 <- sort(bins, decreasing=T)
	cols <- binColors[as.numeric(names(bins2))]
	
	if(type=='bar') {
	barplot(bins2, col=cols, xlab='Orientation Bins', ylab='Surface Area', main=main)
	}
	
	if(type=='pie') {
		ptc <- round(bins2/sum(bins2)*100)
		labels <- paste(paste('Bin', names(bins2), sep='-'), paste(ptc, "%", sep=''), sep=' ')
		pie(bins2, col=cols, labels=labels, main=main)
	}
	
}

