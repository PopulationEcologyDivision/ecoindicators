MeanTLLandings <- function (land=dat,cutoff=0) {
	res <- sqlQuery(channel,paste('select * from indiseas_allcodes2res;'))
	names(res)[1]<-'SPECIES'
		#In Land there duplicate entires for some species which allows for proportions of total landings to be calucaulted  as aggregate(LAND*PROPORTION_OF_LANDINGS~YEAR,data=Land,FUN=sum)
		Land <- merge(land,res)
		TL <- sqlQuery(channel,paste('select * from indiseas_WSS_TL;'))
			TLL <- merge(Land,TL)
			if(cutoff==0) {
			TLL$pp <- TLL$CATCH*TLL$LANDED*TLL$PROPORTION_OF_LANDINGS #this is for the proprtion of different species
			TLL$LL <- TLL$pp*TLL$TL
			}
			if(cutoff>0) {
				TLL$id <- TLL$TL>=cutoff
				TLL$pp <- TLL$CATCH*TLL$LANDED*TLL$PROPORTION_OF_LANDINGS*TLL$id #this is for the proprtion of different species
				TLL$LL <- TLL$pp*TLL$TL*TLL$id
			}
		mTL <- merge(aggregate(LL~YEAR+NAMES,data=TLL,FUN=sum),aggregate(pp~YEAR+NAMES,data=TLL,FUN=sum))
		mTL$mTL <- mTL[,3]/mTL[,4]
		return(mTL[,c('YEAR','NAMES','mTL')])

}
