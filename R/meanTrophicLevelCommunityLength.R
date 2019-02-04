meanTrophicLevelCommunityLength <- function(X,pred.data.table='indiseas_wss_tl',metric='BIOMASS',path=NA) {
	if(is.na(path)) ol <- sqlQuery(channel,paste("select * from ",pred.data.table,sep=""))
	if(!is.na(path)) ol <- read.csv(file.path(path,"extra info","wss_tl_length.csv"),header=T)
	uI <- unique(X$ID)
	out <- numeric()
	mTLc <- sqldf(paste("select ID,flen,species,",metric,",GROUP_NAME,TL from X s, ol t where s.species=t.research and s.flen between t.MINL and t.MAXL;",sep=""))
	for(i in 1:length(uI)) {
	                Y <- mTLc[mTLc$ID==uI[i],]
	                out[i] <- aggregate(Y[metric]*Y['TL'],by=Y['ID'],FUN=sum)[,2]/aggregate(Y[metric],by=Y['ID'],FUN=sum)[,2]
		}
		out <- as.data.frame(cbind(uI,out))
		out[,2] <- as.numeric(out[,2])
		names(out) <- c('ID','MTLCom')
	return(out)
		}
