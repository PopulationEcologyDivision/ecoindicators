makePlots <- function(path,groups,indicator) {
			f <- file.path(path,"output","Combined Indicators")
			fi <- dir(f,full.names=T)
			fi <- fi[grep(groups,fi)]
		
			if(groups=='strat') {
				fi <- fi[grep('esswss',fi,invert=T)]
				fi <- fi[grep('nafo',fi,invert=T)]
				fi <- fi[grep('shelf',fi,invert=T)]
			}
			n <- length(fi)
			a<- list()
			h <- rep(0,n)
			for(i in 1:n) {
			a[[i]] <- read.csv(fi[i],header=T)
			if(any(names(a[[i]])==indicator)) h[i]=1  
			}
			if(sum(h)==2) {
			
			for(i in 1:n) {
			a <- read.csv(fi[i],header=T)
				names(a)[which(names(a)==indicator)] <- paste(indicator,i,sep="")
				if(i==1) {
					ou <- a[,c('YEAR','ID',paste(indicator,i,sep=""))]
					} else {
				ou <- merge(ou,a[,c('YEAR','ID',paste(indicator,i,sep=""))],by=c('YEAR','ID'))
				}
			}
			g <- do.call(rbind,strsplit(fi,paste(f,"/",sep="")))[,2]
			nq 			<- grep('nonq',g)
			setq 		<- grep('setq',g)
			#stratifiedq <- grep('stratifiedq',g)
			
			ID <- unique(ou$ID)
			for(i in 1:length(ID)) {
				ou1 <- ou[ou$ID==ID[i],]
				ylims=c(min(ou1[,3:(2+n)],na.rm=T),max(ou1[,3:(2+n)],na.rm=T))
				plot(ou1[,1],1:nrow(ou1),ylim=ylims,xlab='Year',ylab=indicator,main=ID[i],type='n')
				for(j in 3:ncol(ou1)) {
					lines(ou1[,1],ou1[,j],col=j-2,lwd=1.5)
					}			
				legend('top',c('No Q','Set Q'),bty='n',col=c(nq,setq),cex=.7,ncol=3,lty=1)
				}
			}
			}
			