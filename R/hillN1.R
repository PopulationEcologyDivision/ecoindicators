
hillN1 <- function(X,group=c('FINFISH','ALL'),metric=c('BIOMASS','ABUNDANCE'))  {
	out <- shannon(X=X,group=group,metric=metric)
	out[,2] <- exp(out[,2])
	return(out)
	}
