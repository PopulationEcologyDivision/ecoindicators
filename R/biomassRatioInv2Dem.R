biomassRatioInv2Dem <- function(X, syear=yrs[i]) {
	V <- data.frame(ID=NA,BIOMASS.x=NA,BIOMASS.y=NA,RATIO=NA)
	if(syear>=1999) {
	Y <- resourcePotential(X=X,group='GROUNDFISH',metric='BIOMASS')  
	Z <- resourcePotential(X=X,group='INVERTEBRATES',metric='BIOMASS',yr=syear)  
	V <- merge(Y,Z,all.x=T,by='ID')
	V$RATIO <- V$BIOMASS.y/V$BIOMASS.x
		}
	V <- V[,c('ID','RATIO')]

	return(V)	
	}