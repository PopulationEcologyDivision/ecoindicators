kemptonQ<- function(X, percentiles=c(.25,0.75),minTL=3,metric=c('BIOMASS','ABUNDANCE')) {
		#percentiles is the percentiles for r2 and r1
		#based on Kempton and Taylor 1976 Nature 262
		#minTL sets a minimum trophic level for inclusion in group for all species set minTL=0
		#not length based TL, just any species with TL >3 at any size August 29, 2013 09:19:50 AM 
			if(minTL>0) {
			sp <- sqlQuery(channel,paste("select distinct RESEARCH species from indiseas_wss_tl where tl>",minTL," order by research;",sep=""))[,1]
			X <- X[X$SPECIES %in% sp,]
			}
			
			uI <- unique(X$ID)
			Q.est <- numeric()
			 for(i in 1:length(uI)) {
			 	Y <- X[X$ID==uI[i],]
			 	Y <- Y[order(Y[metric]),metric]   
			 	U <- length(Y)
			 	if(U>2) {
			 	w <- c(round(U*percentiles[1],0),round(U*percentiles[2],0))
			 	if(w[1]==0) w[1]<-1
				Q.est[i] <- U*(percentiles[2]-percentiles[1])/log(Y[w[2]]/Y[w[1]])
				}  else {
				Q.est[i]<-NA
				}
			}
			out <- as.data.frame(cbind(uI,Q.est))
			out[,2] <- as.numeric(out[,2])
			names(out)[1] <-'ID'
			return(out)
	}
#example
#a<-biomassData(q.corr=T)
#a$ID <- paste(a$MISSION,a$SETNO)	
#ew <-kemptonQ(a,metric='ABUNDANCE')

