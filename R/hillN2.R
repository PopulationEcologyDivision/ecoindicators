hillN2 <- function(X,group=c('FINFISH','ALL'),metric=c('BIOMASS','ABUNDANCE'))  {

	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}

	uI <- unique(X$ID)
	hill.est <- numeric()
	 for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]
	 	Y <- Y[order(Y[metric]),metric]   
	 	Y <- Y/sum(Y)
	 	hill.est[i] <- 1/sum(Y^2)
		}
	out <- as.data.frame(cbind(uI,hill.est))
	names(out)[1] <-'ID'
	out[,2] <- as.numeric(out[,2])
	return(out)
	}
