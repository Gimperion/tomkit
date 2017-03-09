
#' Opposite of %in%
#'
#' @return Boolean
#'
#' @examples
#' "test" %notin% c("bar", "foo")
#'
#' @export
`%notin%` <- function(x,y) !(x %in% y)


#' Pastes two strings together
#'
#' @return String
#'
#' @examples
#' "foo" %+% "bar"
#'
#' @export
`%+%` <- function(x,y) paste(x,y,sep="")


#' Adds interactive pause to code.  Waits for user input.
#'
#' @return None
#'
#' @examples
#' ... some code
#' readkey()
#'
#' @export
readkey <- function()
{
    cat ("Press [enter] to continue", fill=TRUE)
    line <- readline()
}

#' String function that adds leading zeroes to digits.
#'
#' @return String
#'
#' @examples
#' leadgr(42, 4)
#'
#' @export
leadgr <- function(x, digits=1){
	formatter <- paste0("%0", digits, 'd')
	return(sprintf(formatter, x))
}

#' Removes leading and trailing spaces.
#'
#' @return String
#'
#' @examples
#' trimall("  this is result   ")
#'
#' @export
trimall <- function(tstring){
	return(gsub("(^ +)|( +$)", "", tstring))
}

#' shortened version of prettyNum with diff defaults
#'
#' @return String
#'
#' @examples
#' pNum(5000000)
#'
#' @export
pNum <- function(x){
	return(prettyNum(x, big.mark=',', preserve.width='individual'))
}
