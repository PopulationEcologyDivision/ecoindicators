biomassRetained <- function(X,metric=ags[2],path) {
	pp <- read.csv(file.path(path,"extra info","predatoryretained.csv"))
	pp <- pp[pp$RETAINED==1,'SPECIES']
	Y <- X[X$SPECIES %in% pp,c('ID','BIOMASS')]
	out <- aggregate(BIOMASS~ID,data=Y,FUN=sum)
	return(out)

}