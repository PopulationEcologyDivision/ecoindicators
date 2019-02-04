biomassData <- function(s.strat=440,e.strat=495,s.year=1970,e.year=2015, p=path,vessel.correction=T) {
# use.length.data.for.biomass is the aggregate from the qcorr script ie sum(flen) data
		yr <- s.year:e.year
		
	#get 4X herring data
		her <- herring.at.Length(path1=p)
		
		
for(i in 1:length(yr)) {
	#at Length
	
		abc <- sqlQuery(channel,paste('select * from mfd_stomach.indiseas_catchability_coeffs'))
				abc[abc=='NULL']<-NA
		outputs<-list()
					m=0
				for (j in 1:nrow(abc)) {
						outputs[[j]]<-biomass_q_adj(species=abc[j,1],fun_group=abc[j,2],q=abc[j,3],len_corr=abc[j,4],year=yr[i])
						}
		out <- as.data.frame(do.call(rbind,outputs))
		
		f <-out[out$SPEC==60 & out$STRAT>=470 & out$STRAT<=495,]
		out <- out[setdiff(rownames(out),rownames(f)),]
		mi <- unique(out$MISSION)				
		h <- her[her$MISSION %in% mi,]
		out <- out[,-which(names(out) %in% c('YDDMMSS', 'XDDMMSS'))]
		out <- rbind(out,h)
		if(vessel.correction) out <- vesselCorr(out)

		fna <- paste(p,"/data/length/",sep="")
		dir.create(fna,recursive=T,showWarnings = F)
    fna <- paste(fna,"num_biom_at_length",yr[i],".RData",sep="")
	  save(out,file=fna,compress=T)
		
	#end of ---at length	
	
			
	#begin aggregate
	
			ag.out <- aggregate(cbind(QBIOMASS,BIOMASS,QABUNDANCE,ABUNDANCE)~YEAR+STRAT+MISSION+SETNO+SPECIES,data=out,FUN=sum)
			
			#remove herring at length
			
			f <-ag.out[ag.out$SPEC==60 & ag.out$STRAT>=470 & ag.out$STRAT<=495,]
			ag.out <- ag.out[setdiff(rownames(ag.out),rownames(f)),]
			ag.out <- ag.out[ag.out$BIOMASS>0,]
			

			dat <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat, to_char(sdate,'yyyy') year, spec,sum(nvl(totno,0)*1.75/i.dist) Abundance,sum(nvl(totwgt,0)*1.75/i.dist) biomass from 
							groundfish.gsinf i, groundfish.gscat c, mfd_stomach.nafo_strat sg where i.mission=c.mission and i.setno=c.setno and i.strat=sg.strat and to_char(sdate,'mm') in ('06','07','08') and
							i.strat between '",s.strat,"' and '",e.strat,"' and type=1 and spec<9000 and to_char(sdate,'yyyy')=",yr[i],"
							group by i.mission,i.setno,i.strat,slat , slong ,to_char(sdate,'yyyy'), spec;",sep=""))
			
			
			
			#error changes
			
			
			dat[dat$BIOMASS>8000 & dat$SPEC !=220,'BIOMASS'] <- 0 #added in November 08, 2013 
			
			
			#Change NA for abundance or biomass based on mean size of animals captured (for invs only)
			
			dat[is.na(dat$ABUNDANCE),'ABUNDANCE'] <- 0
			dat[is.na(dat$BIOMASS),'BIOMASS'] <- 0
			
			#add in the aggregated herrring
			her1 <- herringAggregate(path1=p)
			her1 <- her1[her1$ABUNDANCE>0,]
			f <-dat[dat$SPEC==60 & dat$STRAT>=470 & dat$STRAT<=495,]
			dat <- dat[setdiff(rownames(dat),rownames(f)),]
			mi <- unique(dat$MISSION)				
			h <- her1[her1$MISSION %in% mi,]
			
			dat <- rbind(dat,h)

			
			#handle the missing biomass or abudance data where the other is present
			if(any(dat$ABUNDANCE==0 | dat$BIOMASS==0)) {
				wt <- sqlQuery(channel,paste("select * from mean_wts_for_fill_in;",sep=""))
				dat <- merge(dat,wt,by=c('SPEC'),all.x=T)
				dat[is.na(dat$MEAN_WT_FISH),'MEAN_WT_FISH'] <- 0.5
			if(any(dat$ABUNDANCE==0)) {
				dat$ABUNDANCE[dat$ABUNDANCE==0] <- dat$BIOMASS[dat$ABUNDANCE==0]/dat$MEAN_WT_FISH[dat$ABUNDANCE==0]			
				if(any(dat$BIOMASS==0)) {
				dat$BIOMASS[dat$BIOMASS==0] <- dat$ABUNDANCE[dat$BIOMASS==0]*dat$MEAN_WT_FISH[dat$BIOMASS==0]					
				}
			}
			}
			if(unique(dat$YEAR)==2012 & any(!is.finite(dat$ABUNDANCE))) dat[which(!is.finite(dat$ABUNDANCE)),'ABUNDANCE'] <- 1
			if(any(is.na(dat[,c('BIOMASS','ABUNDANCE')]))) browser()
			dat[dat$BIOMASS==0 ,'BIOMASS']<- 0.01	
			dat[dat$ABUNDANCE==0 ,'ABUNDANCE']<- 1
			dat$QABUNDANCE <- dat$ABUNDANCE
			dat$QBIOMASS <- dat$BIOMASS
			dat$SPECIES <- dat$SPEC
			if(vessel.correction) dat <- vesselCorr(dat)
			dat <- dat[,-which(names(dat)=='SPECIES')]
			
			#add in zero sets
		
			extra.sets <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,to_char(sdate,'yyyy') year from groundfish.gsinf i, nafo_strat sg where 
			i.strat=sg.strat and to_char(sdate,'yyyy') =",yr[i]," and to_char(sdate,'mm') in ('06','07','08') and i.strat between '",s.strat,"' and '",e.strat,"' and type=1;",sep=""))
			s <- unique(dat$SPEC)	
			m <- matrix(0,nrow=dim(extra.sets)[1],ncol=length(s),dimnames=list(c(1:nrow(extra.sets)),s))	
			m <-cbind(extra.sets,m)
				h <- melt(m,id=c("MISSION", "SETNO", "STRAT", "YEAR"))
				names(h)[which(names(h) %in% c('variable','value'))]<- c('SPECIES','BIOMASS')
				h$QBIOMASS <- h$QABUNDANCE <- h$MEAN_WT_FISH <- h$ABUNDANCE<-0
			names(dat)[1] <- 'SPECIES'
			
				l <- rbind(dat,h,all=T)
		
			dat <- l[!duplicated(l[,c('MISSION','SETNO','SPECIES')], fromLast=F),]
			
			#add in the qadjusted data
			dat1 <- dat
			#use.length.data.for.biomass
			#CHANGE October 31, 2013 11:36:51 AM  after discussion with AB wanted this to be the default for spera
			w <- merge(dat,ag.out,by=c('MISSION','SETNO','STRAT','SPECIES','YEAR'),all.x=T)	
			w$ABUNDANCE <-0		
			w$QABUNDANCE <-0		
			w$BIOMASS <-0		
			w$QBIOMASS <-0		
			w$ABUNDANCE 	<- ifelse(is.na(w$ABUNDANCE.y),w$ABUNDANCE.x,w$ABUNDANCE.y)
			w$BIOMASS	 	<- ifelse(is.na(w$BIOMASS.y),w$BIOMASS.x,w$BIOMASS.y)
			w$QABUNDANCE 	<- ifelse(is.na(w$QABUNDANCE.y) ,w$QABUNDANCE.x,w$QABUNDANCE.y)
			w$QBIOMASS	 	<- ifelse(is.na(w$QBIOMASS.y),w$QBIOMASS.x,w$QBIOMASS.y)
			dat <- w[,c('MISSION','SETNO','SPECIES','YEAR','STRAT','ABUNDANCE','BIOMASS','QABUNDANCE','QBIOMASS')]		
			fna <- paste(p,"/data/aggregate/sumq/",sep="")
			dir.create(fna,recursive=T,showWarnings=F)
			save(dat,file=paste(fna,"num_biom",yr[i],".RData",sep=""))
			
			#not length data for biomass
			dat <- dat1
			w <- merge(dat,ag.out,by=c('MISSION','SETNO','STRAT','SPECIES','YEAR'),all.x=T)
			w[is.na(w)] <-0
			w$QBIOMASS <- with(w,ifelse(QBIOMASS.x<QBIOMASS.y,QBIOMASS.y,QBIOMASS.x))
			w$QABUNDANCE <- with(w,ifelse(QABUNDANCE.x<QABUNDANCE.y,QABUNDANCE.y,QABUNDANCE.x))
		w <- w[,c('MISSION','SETNO','SPECIES','YEAR','STRAT','ABUNDANCE.x','BIOMASS.x','QABUNDANCE','QBIOMASS')]
		names(w) <- c('MISSION','SETNO','SPECIES','YEAR','STRAT','ABUNDANCE','BIOMASS','QABUNDANCE','QBIOMASS')
			dat <- w
			fna <- paste(p,"/data/aggregate/nosumq/",sep="")
			dir.create(fna,recursive=T,showWarnings=F)
			save(dat,file=paste(fna,"num_biom",yr[i],".RData",sep=""))
			
			
			
	
		print(yr[i])
		
		#length weight data
		fna <- file.path(path,"data","lenwgt")
		dir.create(fna,recursive=T,showWarnings=F)
		wt <- sqlQuery(channel,paste("select distinct strat,spec species,flen,fwt from groundfish.gsinf i, groundfish.gsdet d where i.mission=d.mission and i.setno=d.setno and to_char(sdate,'yyyy') = ",yr[i]," and to_char(sdate,'mm') in ('06','07','08') and strat between '440' and '495' and fwt is not null and flen is not null;",sep=""))
		save(wt,file=paste(fna,"/lw",yr[i],".Rdata",sep=""),compress=T)


	#end all species data w/no length
	rm(dat,w)
		}	
	}					        

##---------------S