meanMaxL <- function(X,table.of.length.data='INDISEAS_MAX_LENGTH',metric=c('BIOMASS','ABUNDANCE'),path=NA) {
		#this indicator is for finfish only
		#X is input data
		
		if(is.na(path)) 	len <- sqlQuery(channel,paste("select * from ",table.of.length.data,";",sep=""))
		if(!is.na(path)) 	len <- read.csv(file.path(path,"extra info",paste(table.of.length.data,".csv",sep="")))
		X <- merge(X,len,by='SPECIES')
		uI <- unique(X$ID) 	
		mmL <-numeric()
		for(i in 1:length(uI)) {
			Y <- X[X$ID==uI[i],]
			mmL[i] <- sum(Y[metric]*Y['MAXLEN99'])/sum(Y[metric])	
		   }
		   out <- as.data.frame(cbind(uI,mmL))
		   names(out)[1] <-'ID'
		   out[,2] <- as.numeric(out[,2])
		   return(out)		
	}