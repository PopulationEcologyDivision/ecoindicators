	meanMaxAge <- function(X,table.of.age.data='INDISEAS_MAX_AGE',metric=c('BIOMASS','ABUNDANCE')) {
		#this indicator is for finfish and squid only
		#X is input data
		
		uI <- unique(X$ID) 	
		len <- sqlQuery(channel,paste("select SPECIES, MAXAGE from ",table.of.age.data,";",sep=""))
		X <- merge(X,len,by='SPECIES')
			mmL <-numeric()
			for(i in 1:length(uI)) {
				Y <- X[X$ID==uI[i],]
				if(nrow(Y)>1) {
				mmL[i] <- sum(Y[metric]*Y['MAXAGE'])/sum(Y[metric])	
				}
				else {
				mmL[i]<-NA
				}
			   }
		   out <- as.data.frame(cbind(uI,mmL))
		   names(out)[1] <-'ID'
		   out[,2] <- as.numeric(out[,2])
		   return(out)		
	}