
#' Opposite of %in%
#'
#' @return Boolean
#'
#' @examples
#' "test" %notin% c("bar", "foo")
#'
#' @export
`%notin%` <- function(x,y) !(x %in% y)



#' pastes two strings together
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

#' Cleans up names of imported data frames
#'
#' @return data.frame
#'
#' @examples
#' cleanNames(data)
#'
#' @export
cleanNames <- function(x){
	require(magrittr)
	names(x) <- names(x) %>%
		iconv("UTF-8", "UTF-8", sub="") %>%
		gsub(pattern="\\.", replacement=" ") %>%
		trimall() %>%
		gsub(pattern=" +", replacement="_") %>%
		tolower()
	return(x)
}

#' functional way of returning specific part of data object
#'
#' @return Any
#'
#' @examples
#' returnPart(data, objectName)
#'
#' @export
returnPart <- function(x, y){
	eval(parse(text=sprintf("x$%s", y)))
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

#' Creates a string with n number of tabs
#'
#' @return String
#'
#' @examples
#' indent(6)
#'
#' @export
indent <- function(n){
	return(paste(rep("\t", n), collapse=''))
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

#' Replaces values based on if/else boolean logic.
#' supports piping a la dplyr
#'
#' @return Vector
#'
#' @examples
#' c("a","b", "x", "x", "a") %>%
#' 	ifel_repl(. == "x", "replaced!")
#'
#' @export
ifel_repl <- function(data, boolean, replacement){
	ifelse(boolean, replacement, data)
}


# deprecated
# increment <- function(x)
# {
# 	eval.parent(substitute(x <- x + 1))
# }
#
# up <- function(x){
# 	eval.parent(substitute(x <- x + 1))
# }
# down <- function(x){
# 	eval.parent(substitute(x <- x - 1))
# }


## PLOT MULTIPLE DISTRIBUTIONS
# plot.multi.dens <- function(s, toplb="")
# {
# 	junk.x = NULL
# 	junk.y = NULL
# 	for(i in 1:length(s))
# 	{
# 		junk.x = c(junk.x, density(s[[i]],na.rm=TRUE)$x)
# 		junk.y = c(junk.y, density(s[[i]],na.rm=TRUE)$y)
# 	}
# 	xr <- range(junk.x)
# 	yr <- range(junk.y)
# 	plot(density(s[[1]],na.rm=TRUE), xlim = xr, ylim = yr, main = toplb)
# 	for(i in 1:length(s))
# 	{
# 		lines(density(s[[i]], na.rm=TRUE), xlim = xr, ylim = yr, col = i)
# 	}
# }
#
# ## FINDS INTEGER MODE OF A STRING SET
# mhack <- function(x){
# 	temp <- table(as.vector(x))
# 	if(length(subset(x, is.na(x)))/length(x)>.5){
# 		return(NA)
# 	} else{
# 		return(as.integer(names(temp)[temp == max(temp)]))
# 	}
# }
#
# ## FINDS STRING MODE OF A STRING SET
# mhack2 <- function(x){
# 	temp <- table(as.vector(x))
# 	if(length(x[is.na(x)])>500){
# 		return(NA)
# 	} else{
# 		return(names(temp)[temp == max(temp)])
# 	}
# }
#
# ## Exporting Stuff to JSON
# checkna <- function(x){
# 	if(is.na(x)){
# 		return('null')
# 	}
# 	return(as.character(x))
# }
#
# checkna_str <- function(x){
# 	if(is.na(x)| length(x) ==0){
# 		return('null')
# 	}
# 	return('"' %+% x %+%'"')
# }
#
# sampdf <- function(x,y) {
# 	x[sample(1:nrow(x),y),]
# }
#
# make_null <- function(x){
# 	if(length(x) == 0) {x <- 'null'}
# 	else if (is.na(x) | is.null(x)){x <- 'null'}
# 	else return(x)
# }
#
#
# ## Add Auto line breaks to long strings.
# add_breaks <- function(x, n=17){
#   if(nchar(x) < n){
# 		return(x)
# 	}
#
# 	i <- n-1
# 	while(i > 5){
# 		if(substr(x, i, i) == " "){
# 			substr(x,i,i) <- "\n"
#
# 			if(nchar(x) - i > n + 2){
# 				return(paste0(substr(x, 1,i),add_breaks(substr(x, i+1, nchar(x)),n)))
# 			}
# 			return(x)
# 		}
# 		down(i)
# 	}
# 	return(x)
# }
#
#
# ## formatting
# fix_edates <- function(dat, dcols){
#     for(i in dcols){
#         dat[,i] <- as.Date(dat[,i], format="%m/%d/%Y")
#     }
#     return(dat)
# }
#
# fix_names <- function(x){
#     x <- tolower(x)
#     x <- gsub("\\."," ",x)
#
#     return(x)
# }
#
# ceiling100 <- function(x){
# 	ceiling(max(x)/100)*100
# }
