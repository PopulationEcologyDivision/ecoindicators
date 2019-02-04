
margalef<- function(X,group=c('FINFISH','ALL'),metric=c('BIOMASS','ABUNDANCE')) {
#needed to limit the range of margalef to emliminate soem of our sets where biomass or abundances are 
#extremely low ie~0 and there are several species as values go crazy November 26, 2012 01:02:56 PM AMC
	if(group=='FINFISH') {
		X <- X[as.numeric(X$SPECIES)<1000,]
		}
		X <- X[X[metric]>0,]
	uI <- unique(X$ID)
	MDI.est <- numeric()
	 for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]	 	
	 	Y <- Y[order(Y[metric]),metric]   
	 	U <- length(Y)
	 	
	 	oo <- (U-1)/log(sum(Y)) 
	 	if(oo>0 && oo < 50) {
	 		MDI.est[i] <- oo
	 		}	else {
	 		MDI.est[i] <- NA
	 		}
	 	}
	out <- as.data.frame(cbind(uI,MDI.est))
	names(out)[1] <-'ID'
	out[,2] <- as.numeric(out[,2])
	return(out)
}
