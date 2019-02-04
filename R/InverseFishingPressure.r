InverseFishingPressure <- function(path,land=dat,group,groups,qadj=T,qadjPostStrat=qadjPostStrat) {
	out <- FishingPressure(path,land,group,groups,qadj=qadj,qadjPostStrat=qadjPostStrat)
	out$FP <- 1/out$FP
	return(out)
}