margalefSpeciesRichness <- function(X,group=c('FINFISH','ALL'),metric=c('BIOMASS','ABUNDANCE'))  {

	if(group=='FINFISH') {
		X <- X[X$SPECIES<1000,]
		}
	if(group=='GROUNDFISH' )  X <- X[X$SPECIES %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,115,116,117,118,200,201,202,203,204,205,206,207,208,209,210,211,220,221,300,301,304,310,320,340,350,400,620:650),]    	

	uI <- unique(X$ID)
	marsr.est <- numeric()
	 for(i in 1:length(uI)) {
	 	Y <- X[X$ID==uI[i],]
	 	Y <- Y[order(Y[metric]),metric]   
	 	marsr.est[i] <- (length(Y)-1)/log(sum(Y))
	 	if(marsr.est[i]>20 || marsr.est[i]<0) marsr.est[i]<-NA
	 	}
	out <- as.data.frame(cbind(uI,marsr.est))
	names(out)[1] <-'ID'
	out[,2] <- as.numeric(out[,2])
	return(out)
	}
