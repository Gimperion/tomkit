#' Creates a blank plot canvas based on dimensions provided. Does not return anything.
#'
#' @return NULL
#'
#' @examples
#' blankPlot(x=c(0,10),y=c(0,20))
#'
#' @export

blankPlot <- function(...){
	plot(..., type='n', bty='n',
		xaxt='n', yaxt='n', xlab="", ylab="")
}

