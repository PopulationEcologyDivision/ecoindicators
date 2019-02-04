	invCVBiomass <- function(X,window=5)  {
			uI <- unique(X$ID) 
			out <- list()
				for(i in 1:length(uI)) {
				 Y <- X[X$ID==uI[i],]
				  if(nrow(Y)>window) {
				 Yp <- aggregate(BIOMASS~YEAR,data=Y,FUN=sum)
				 aw <- data.frame(YEAR=Yp['YEAR'],BIOMASS=1/ movingStatistics(Yp[,'BIOMASS'],n=window,stat='cv'),IDS=uI[i])
				 my <- min(aw$YEAR)
				 xy <- max(aw$YEAR)
				 out[[i]] <- aw[!aw$YEAR %in% c(my,my+1,xy-1,xy),]
				 }
				}
					return(do.call(rbind,out))
			   }
