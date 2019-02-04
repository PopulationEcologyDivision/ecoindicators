rmAllBut <- function(x) {
	rm(list=setdiff(ls(pos=1), x),pos=1)
}