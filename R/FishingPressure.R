FishingPressure <- function(path,land=dat,group,groups,qadj=T,qadjPostStrat=qadjPostStrat) {
	la  <- LandByGroup(land,group)
	out <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(group,'BIOMASS',user.defined=F),groups=groups,qadjusted=qadj,saveIndicatorData=F,yrs=1970:2015,qadjPostStrat=qadjPostStrat)
	names(la)[which(names(la)=='NAMES')] <- 'ID'
	fp <- merge(out,la,by=c('YEAR','ID'))
	fp$FP <- fp$CATCH/fp$BIOMASS
	return(fp[,c('YEAR','ID','FP')])
	}