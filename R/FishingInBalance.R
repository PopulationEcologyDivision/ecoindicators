FishingInBalance<- function (land=dat,TE=0.1) {
	mTL <- MeanTLLandings(land=land)
	ll <- aggregate(CATCH~YEAR+NAMES,data=land,FUN=sum)
	l0 <- aggregate(CATCH~NAMES,data=ll[ll$YEAR %in% 1968:1970,],FUN=mean)
	mTL0 <- aggregate(mTL~NAMES,data=mTL[mTL$YEAR %in% 1968:1970,],FUN=mean)
	
	nn <- unique(l0$NAMES)
	outs <- list()
		for(i in 1:length(nn)) {
			lan <- ll[ll$NAMES==nn[i],'CATCH']
			yy <- ll[ll$NAMES==nn[i],'YEAR']
			mtl <- mTL[mTL$NAMES==nn[i],'mTL']
			mtlo <- mTL0[mTL0$NAMES==nn[i],'mTL']
			llo <- l0[l0$NAMES==nn[i],'CATCH']
		outs[[i]] <- data.frame(NAMES=rep(nn[i],length(lan)),YEAR=yy,INDI=log(lan*(1/TE)^mtl)-log(llo*(1/TE)^mtlo))	
		}
	return(do.call(rbind,outs))
}
