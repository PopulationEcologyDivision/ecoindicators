meanTrophicLevelCommunity <- function(X,pred.data.table='indiseas_wss_tl',metric='BIOMASS') {
	ol <- sqlQuery(channel,paste("select research species,avg(TL) from ",pred.data.table," group by research;",sep=""))
	X <- merge(X,ol,by='SPECIES')
	uI <- unique(X$ID)
	out <- numeric()
	for(i in 1:length(uI)) {
	                Y <- X[X$ID==uI[i],]
	                out[i] <- aggregate(Y[metric]*Y['AVG(TL)'],by=Y['ID'],FUN=sum)[,2]/aggregate(Y[metric],by=Y['ID'],FUN=sum)[,2]
		}
		
		out <- as.data.frame(cbind(uI,out))
		out[,2] <- as.numeric(out[,2])
		names(out) <- c('ID','MTLCom')
	return(out)
		}
