speciesRichness <- function(X,group=c('FINFISH','ALL'),grps)  {

	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
	
	uI <- unique(X$ID)
	sr.est <- numeric()
	for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]
	 	sr.est[i] <- length(unique(Y$SPECIES))
		}
	out <- as.data.frame(cbind(uI,sr.est))
	out[,2] <- as.numeric(out[,2])
	return(out)
	}
