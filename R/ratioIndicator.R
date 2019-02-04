#ratio between groups
ratioIndicator <- function(X,group,metric,user.defined) {
	Y <- resourcePotential(X=X,group=group[1],metric=metric,user.defined=user.defined)  
	Z <- resourcePotential(X=X,group=group[2],metric=metric,,user.defined=user.defined)  
	V <- merge(Y,Z,all.x=T,by='ID')
	V$RATIO <- V$BIOMASS.y/(V$BIOMASS.x+V$BIOMASS.y)
	return(V)	
	}