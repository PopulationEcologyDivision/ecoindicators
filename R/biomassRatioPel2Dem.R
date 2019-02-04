biomassRatioPel2Dem <- function(X) {
		
	Y <- resourcePotential(X=X,group='GROUNDFISH',metric='BIOMASS')  
	Z <- resourcePotential(X=X,group='PELAGIC',metric='BIOMASS')  
	V <- merge(Y,Z,all.x=T,by='ID')
	V$RATIO <- V$BIOMASS.y/V$BIOMASS.x
	V <- V[,c('ID','RATIO')]
	return(V)
	}	
	