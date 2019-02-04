largeFishIndicator <- function(X,metric=c('BIOMASS','ABUNDANCE'),large.fish=35) {
	uI <- unique(X$ID)
	mL <- numeric()
		for(i in 1:length(uI)) {
	Y <- X[X$ID==uI[i],]
	id <- Y$FLEN>=large.fish
		mL[i] <- sum(Y[id,metric])/sum(Y[,metric])
	}
	out <- as.data.frame(cbind(uI,mL))
	out[,2] <- as.numeric(out[,2])
	names(out) <- c('ID','mLc')
	return(out)
	}