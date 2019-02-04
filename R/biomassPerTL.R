biomassPerTL <- function(X,table.of.trophic.level.data='INDISEAS_WSS_TL',metric=c('BIOMASS','ABUNDANCE'), TL.grouping=1,path=NA) {
		#X is input data
		#not length based
		#breaks can be divided into three levels of grouping
		uI <- unique(X$ID) 	
		if(is.na(path)) TLS 	<- sqlQuery(channel,paste("select research species, avg(TL) TL from ",table.of.trophic.level.data," group by research;",sep=""))
		if(!is.na(path)) TLS 	<- read.csv(file.path(path,"extra info",paste(table.of.trophic.level.data,".csv",sep="")))
		breaks <- seq(1,10,by=TL.grouping)
		TLS['TL'] <- breaks[findInterval(TLS[,'TL'],breaks)]
		
		X <- merge(X,TLS,by='SPECIES')		
		mmL <-list()
		
		for(i in 1:length(uI)) {
			Y <- X[X$ID==uI[i],]
			mmL[[i]] <- aggregate(Y[metric],by=c(Y[c('ID','TL')]),FUN=sum)
		   }
		   out <- as.data.frame(do.call(rbind,mmL))
		   return(out)		
	}