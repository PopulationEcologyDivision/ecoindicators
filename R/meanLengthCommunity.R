meanLengthCommunity <- function(X,metric=c('BIOMASS','ABUNDANCE')) {
	uI <- unique(X$ID)
	mL <- numeric()
	for(i in 1:length(uI)) {
	Y <- X[X$ID==uI[i],]
		mL[i] <- sum(Y[,'FLEN']*Y[,metric])/sum(Y[,metric])
	}
	out <- as.data.frame(cbind(uI,mL))
	out[,2] <- as.numeric(out[,2])
	names(out) <- c('ID','mLc')
	return(out)
}