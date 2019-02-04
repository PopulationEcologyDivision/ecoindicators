shannon <- function(X,group=c('FINFISH','ALL'),metric=c('BIOMASS','ABUNDANCE'))  {
	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
	uI <- unique(X$ID)
	sh.est <- numeric()
	for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]
	 	Y <- Y[order(Y[metric]),metric]   
	 	Y <- Y/sum(Y)
	 	sh.est[i] <- -sum(Y*log(Y))
		}
	out <- as.data.frame(cbind(uI,sh.est))
	names(out)[1] <-'ID'
	out[,2] <- as.numeric(out[,2])
	return(out)
	}

