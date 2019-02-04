bothDuplicated <- function(x) {
	a <- which(duplicated(x) | duplicated(x,fromLast=T))
	return(a)
}