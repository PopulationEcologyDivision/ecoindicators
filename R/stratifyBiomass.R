stratifyBiomass <- function(path=path,s.year=1970,e.year=2016,lengthBased=T,qadjusted=T, biomass.data.from.qcorr=T, qadjPostStrat=T,areas= c('esswss','nafo','shelf','strat')	) {
				st.weights  <- read.csv(file.path(path,"extra info","stratweights.csv"))

				out.fp <- file.path(path,"data","stratified")
				yrs <- s.year:e.year
		
		for(i in 1:length(yrs)) {
			print(yrs[i])
			#trawlable units differnce (fanning 1985)
					if(yrs[i]<=1981) st.weights$TUNITS 	<- st.weights$AREA/((35./6080.2)*1.75)
					if(yrs[i]>1981) st.weights$TUNITS 	<- st.weights$AREA/((41./6080.2)*1.75)
		
				if(!lengthBased) {
							u <- 'notlengthbased'
	
					#using either the biomass from q corrected data or from the survey gscat tables (!biomass.data.from.qcorr is the same as vdc)	
							if(biomass.data.from.qcorr) { fna 	<- paste(path,"/data/aggregate/sumq/num_biom",yrs[i],".RData",sep=""); u <- paste(u,'biomassfromqcorr',sep="")	}	
							if(!biomass.data.from.qcorr) { fna 	<- paste(path,"/data/aggregate/nosumq/num_biom",yrs[i],".RData",sep=""); u <- paste(u,'notbiomassfromqcorr',sep="")	}	
							load(fna)
							dat$SPECIES <- as.numeric(dat$SPECIES)
					#Begin Stratified Estimates
							if(any(dat$YEAR<1970)) { dat <- dat[-which(dat$YEAR<1970),] }
		      
							
					  
					  
					if(!qadjPostStrat) {
							u <- paste(u,'notqadjpoststrat',sep="")		
							if(qadjusted) {
									dat <- dat[,-which(names(dat) %in% c('BIOMASS','ABUNDANCE'))]
									names(dat)[which(names(dat) %in% c('QBIOMASS'))] <- c('BIOMASS')
									names(dat)[which(names(dat) %in% c('QABUNDANCE'))] <- c('ABUNDANCE')
									u <- paste(u,'qadj',sep="") 
									}
							if(!qadjusted) {
									dat <- dat[,-which(names(dat) %in% c('QBIOMASS','QABUNDANCE'))]
									u <- paste(u,'notqadj',sep="") 
									}
				
						#strata means	
							
							dat <- aggregate(cbind(BIOMASS,ABUNDANCE)~YEAR+STRAT+SPECIES,data=dat,FUN=mean)	
					
						#add in the strata weightings for the appropriate groups
							dat <- merge(dat,st.weights,by='STRAT',all.x=T)	
							out <- dat
						#strata totals				
							out$BIOMASS 	<- out$BIOMASS*out$TUNITS
							out$ABUNDANCE 	<- out$ABUNDANCE*out$TUNITS
							out$SPECIES 	<- as.numeric(out$SPECIES)
							ou <- out[out$BIOMASS!=0,]	#remove the zeros as all species were assumed across all areas	
					
					for(j in 1:length(areas)) {
						out <- defineGroups(ou,gp=areas[j])
						out <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID,data=out,FUN=sum)
						fp1 <- file.path(out.fp,u,areas[j])
						dir.create(fp1,recursive=T,showWarnings=F)
						save(out,file=file.path(fp1,paste(yrs[i],".RData",sep="")))
						}
					}
		
				if(qadjPostStrat) {
						u <- paste(u,'qadjpoststratqadj',sep="")		
						fna <- paste(path,"/data/length/num_biom_at_length",yrs[i],".RData",sep="")
						load(fna)
		
					#Make Stratified Estimates of species to be q'd
							if(any(out$YEAR<1970)) { out <- out[-which(out$YEAR<1970),] }
						#remove the q's at set level
							out <- out[,-which(names(out) %in% c('QBIOMASS','QABUNDANCE'))]
						#strata means
							out <- aggregate(cbind(BIOMASS,ABUNDANCE)~YEAR+STRAT+SPECIES+FLEN,data=out,FUN=mean)	
				
						#add in the strata weightings for the appropriate groups
							out <- merge(out,st.weights,by='STRAT',all.x=T)	#combine to ensure only sampled strat are included
							out$BIOMASS <- out$TUNITS*out$BIOMASS			
							out$ABUNDANCE <- out$TUNITS*out$ABUNDANCE	
							out$SPECIES <- as.numeric(out$SPECIES)
										
							ou <- out[out$BIOMASS>0,]
					
					#--stratify the no q'd species
									dat <- dat[,-which(names(dat) %in% c('BIOMASS','ABUNDANCE'))]
									names(dat)[which(names(dat) %in% c('QBIOMASS'))] <- c('BIOMASS')
									names(dat)[which(names(dat) %in% c('QABUNDANCE'))] <- c('ABUNDANCE')
							#strata means				
									dat <- aggregate(cbind(BIOMASS,ABUNDANCE)~YEAR+STRAT+SPECIES,data=dat,FUN=mean)	
							#add in the strata weightings for the appropriate groups
									dat <- merge(dat,st.weights,by='STRAT',all.x=T)	
							#strata totals				
									dat$BIOMASS 	<- dat$BIOMASS*dat$TUNITS
									dat$ABUNDANCE 	<- dat$ABUNDANCE*dat$TUNITS
									dat$SPECIES 	<- as.numeric(dat$SPECIES)
									da <- dat[dat$BIOMASS!=0,]	#remove the zeros as all
						
				for(j in 1:length(areas)) {
						out <- defineGroups(ou,gp=areas[j])
						ins <- which(out$SPECIES==60 & out$STRAT>469)
						out[ins,'BIOMASS'] <- out[ins,'BIOMASS']*0.025 #down weight 4X by qadj
						out[ins,'ABUNDANCE'] <- out[ins,'ABUNDANCE']*0.025 #down weight 4X by qadj
						out <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID+FLEN,data=out,FUN=sum)
						out <- qBiomassPostStrat(out,path=path)
						out <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID,data=out,FUN=sum)
						sp <- unique(out$SPECIES)
				#add in not qd species
						dat <- defineGroups(da,gp=areas[j])
						dat <- dat[!dat$SPECIES %in% sp,]
						dat <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID,data=dat,FUN=sum)
				#combine two files
						out <- rbind(out,dat)
						fp1 <- file.path(out.fp,u,areas[j])
						dir.create(fp1,recursive=T,showWarnings=F)
						save(out,file=file.path(fp1,paste(yrs[i],".RData",sep="")))
					}
				}
			}
	if(lengthBased) {
		u <- 'lengthbased'
		u <- paste(u,'biomassfromqcorr',sep="")		
		fna <- paste(path,"/data/length/num_biom_at_length",yrs[i],".RData",sep="")
		load(fna)
		
			#add in the other species that don't have length info
				fna <- paste(path,"/data/aggregate/sumq/num_biom",yrs[i],".RData",sep="")		
				load(fna)
				if(any(dat$YEAR<1970)) { dat <- dat[-which(dat$YEAR<1970),] }
				spo <- unique(out$SPECIES)
				dat <- dat[!dat$SPECIES %in% spo,]
				dat$FLEN <- -99
				out <- rbind(out,dat)
			
		dat <- out
		rm(out)
	
		#Make Stratified Estimates
		if(any(dat$YEAR<1970)) { dat <- dat[-which(dat$YEAR<1970),] }
		if(!qadjPostStrat) {
		u <- paste(u,'notqadjpoststrat',sep="")		
			if(qadjusted) {
						dat <- dat[,-which(names(dat) %in% c('BIOMASS','ABUNDANCE'))]
						names(dat)[which(names(dat) %in% c('QBIOMASS'))] <- c('BIOMASS')
						names(dat)[which(names(dat) %in% c('QABUNDANCE'))] <- c('ABUNDANCE')
						u <- paste(u,'qadj',sep="") 
						}
				
			if(!qadjusted) {
						dat <- dat[,-which(names(dat) %in% c('QBIOMASS','QABUNDANCE'))]
						u <- paste(u,'notqadj',sep="") 
						}

		
					dat <- aggregate(cbind(BIOMASS,ABUNDANCE)~YEAR+STRAT+SPECIES+FLEN,data=dat,FUN=mean)	
				
					#add in the strata weightings for the appropriate groups
					dat <- merge(dat,st.weights,by='STRAT',all.x=T)	#combine to ensure only sampled strat are included
							
				dat$BIOMASS <- dat$TUNITS*dat$BIOMASS			
				dat$ABUNDANCE <- dat$TUNITS*dat$ABUNDANCE	
				dat$SPECIES <- as.numeric(dat$SPECIES)
				dat <- dat[dat$BIOMASS>0,]
				ou <- dat
				for(j in 1:length(areas)) {
						out <- defineGroups(ou,gp=areas[j])
						out <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID+FLEN,data=out,FUN=sum)
						fp1 <- file.path(out.fp,u,areas[j])
						dir.create(fp1,recursive=T,showWarnings=F)
						save(out,file=file.path(fp1,paste(yrs[i],".RData",sep="")))
					}
				}
				
		if(qadjPostStrat) {	
		u <- paste(u,'qadjpoststratqadj',sep="")		
						dat <- dat[,-which(names(dat) %in% c('QBIOMASS','QABUNDANCE'))]
						dat <- aggregate(cbind(BIOMASS,ABUNDANCE)~YEAR+STRAT+SPECIES+FLEN,data=dat,FUN=mean)	
						dat <- merge(dat,st.weights,by='STRAT',all.x=T)	#combine to ensure only sampled strat are included
						dat$BIOMASS <- dat$TUNITS*dat$BIOMASS			
						dat$ABUNDANCE <- dat$TUNITS*dat$ABUNDANCE	
						dat$SPECIES <- as.numeric(dat$SPECIES)
						dat99 <- dat[dat$FLEN==-99,]
						dat <- dat[!dat$FLEN==-99,]
				
				for(j in 1:length(areas)) {
						out <- defineGroups(dat,gp=areas[j])
						out99 <- defineGroups(dat99,gp=areas[j])
						ins <- which(out$SPECIES==60 & out$STRAT>469)
						out[ins,'BIOMASS'] <- out[ins,'BIOMASS']*0.025 #down weight 4X by qadj
						out[ins,'ABUNDANCE'] <- out[ins,'ABUNDANCE']*0.025 #down weight 4X by qadj
						out <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID+FLEN,data=out,FUN=sum)
						out <- out[out$BIOMASS>0,]
						out99 <- out99[out99$BIOMASS>0,]
						out99 <- aggregate(cbind(BIOMASS,ABUNDANCE)~SPECIES+ID+FLEN,data=out99,FUN=sum)
						out <- qBiomassPostStrat(out,path=path)
						out <- rbind(out,out99)
						fp1 <- file.path(out.fp,u,areas[j])
						dir.create(fp1,recursive=T,showWarnings=F)
						save(out,file=file.path(fp1,paste(yrs[i],".RData",sep="")))
					}		
			}
	}
}
}