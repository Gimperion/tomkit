
#' Cleans up names of imported data frames
#'
#' @return data.frame
#'
#' @examples
#' cleanNames(data)
#'
#' @export
cleanNames <- function(x){
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
	eval(parse(text=sprintf("x$`%s`", y)))
}


#' Replaces values based on if/else boolean logic.
#' supports piping
#'
#' @return Vector
#'
#' @examples
#' c("a","b", "x", "x", "a") %>% ifel_repl(. == "x", "replaced!")
#'
#' @export
ifel_repl <- function(x, boolean, replacement){
	ifelse(boolean, replacement, x)
}


#' Creates a mapping vector based on data.frame columns
#' supports piping a la dplyr
#'
#' @return Vector
#'
#'
#' @export
createMap <- function(x, base, name){
    x[,base] %>%
        c() %>%
        setNames(x[,name])
}

#' Prints something then passes object along
#' Text marker for long piped processes
#'
#' @return Anything
#'
#'
#' @export
pipeMarker <- function(x, text=x){
    cat(text, fill=TRUE)
    return(x)
}


#' Calls an anonymous function if conditions are met
#'
#'
#' @return Anything
#'
#'
#' @export
exec_if <- function(x, cond, .f){
	require(purrr)
	if(cond) exec(as_mapper(.f), x, environment()) else x
}
