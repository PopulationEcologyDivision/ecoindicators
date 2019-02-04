IVILandings <- function (land=dat,path=NA) {
	
		if(is.na(path)) res <- sqlQuery(channel,paste('select * from indiseas_allcodes2res;'))
		if(!is.na(path)) res <- read.csv(file.path(path,"extra info","indiseas_allcodes2res.csv"))
		
	names(res)[1]<-'SPECIES'
		#In Land there duplicate entires for some species which allows for proportions of total landings to be calucaulted  as aggregate(LAND*PROPORTION_OF_LANDINGS~YEAR,data=Land,FUN=sum)
	Land <- merge(land,res)
		if(is.na(path)) IVI <- sqlQuery(channel,paste('select * from indiseas_IVI;'))
		if(!is.na(path)) IVI <- read.csv(file.path(path,"extra info","indiseasIVI.csv"))
		IV <- merge(Land,IVI)
		IV$IV1 <- IV$CATCH*IV$PROPORTION_OF_LANDINGS*IV$IVI
		IV$CI <- IV$CATCH*IV$PROPORTION_OF_LANDINGS
		
		IV2 <- merge(aggregate(IV1~YEAR+NAMES,data=IV,FUN=sum),aggregate(CI~YEAR+NAMES,data=IV,FUN=sum))
		IV2$INDI <- IV2[,3]/IV2[,4]
		return(IV2[,c('YEAR','NAMES','INDI')])
}
