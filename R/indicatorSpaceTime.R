	getPackage('RODBC')
	getPackage('PBSmapping')
	getPackage('RColorBrewer')
	getPackage('sqldf')
	getPackage('reshape')
	options(stringsAsFactors=F)
	
#changelog
# Moved the stratification estimates out of the indicatorSpaceTime into another function stratifyBiomass.r and saving the output to increase speed of extraction
##-------
#Details#
###Function Arguements
	#----path sets up the highest level folder for all analyses
	#----groups are your areas for aggregation see function defineGroups.r for potential groupings
	#----indicator naming convention --- landings indicators are upper camel case and biomass indicators are lower camel case			
	#----ags are specific indicator specific arguments
	#----	ags[1]=species grouping
	#----	ags[2]=c('BIOMASS','ABUNDANCE')
	#----	ags[3]=user.defined - allows for the definition of a specific grouping not previously identified in ags[1]
	#----	ags[4]=if ratioIndicator this is the second species in the ratio
	#----qadjsuted indicates whether indicator data uses the length based q or non q adjustments based on Harley and Myers 2001
	#----saveIndicatorData defines whether you save the estimated indicator
	#----biomass.data.from.qcorr indicates whether the data you use comes from the qcorrected script or not
	#----qadjPostStrat indicates whether q adjustments are applied at the set level or at the post stratified level
	
##Data useages
#All 4X herring data comes from Araju and Bundy 2013 VPA adjusted with 2009-2013 being repeated data
#Area specific groupings are based on summer strata not any other polygons
#fisheries data are based on the NAFO subunits and groupings provided in the landings databases
#survey indices use fannings 1985 vessel corrections which are two phase as indicated in his paper catch rate correction then wing spread corrections (verified by SJ Smith Sept 2013)




	indicatorSpaceTime <- function( yrs=1970:2015,
							path='C:/RProjects/indicatorSpaceTime',
							groups=c('strat','nafo','shelf','esswss','indiseas', 'esswssbof', 'esswssbof_mpa'),
							indicator=c('kemptonQ',	'margalef',	'shannon','hillN1',	'hillN2',	
									'pielouSpeciesEvenness','margalefSpeciesRichness',	'speciesRichness',	'resourcePotential','meanMaxL','biomassPerTL',	'invCVBiomass',
									'meanMaxAge',	'communityFultonK',	'largeSpeciesIndicator','meanTrophicLevelCommunity','ratioIndicator','biomassRetained',
									'predatoryFish','biomassRatioInv2Dem','biomassRatioPel2Dem','meanLengthCommunity','meanTrophicLevelCommunityLength',
									'IVILandings','MeanTLLandings','FishingInBalance','TrophicBalanceIndex','LandingsDiversity'),
							ags=c('ALL','BIOMASS',user.defined=F),
							qadjusted=T, 
							saveIndicatorData=T, 
							biomass.data.from.qcorr=T,
							qadjPostStrat=T)
						
						 {
						 print(paste(indicator,groups))
	if(substr(indicator,1,1) !=toupper(substr(indicator,1,1))){		#this makes sure they are nonlandings functions		
	
	outputs <- list()					
	for(i in 1:length(yrs)) {
	print(yrs[i])
				
		
		if(indicator %in% c('kemptonQ',	'margalef',	'shannon','hillN1',	'hillN2','biomassPerTL',	
									'pielouSpeciesEvenness','margalefSpeciesRichness',	'speciesRichnessMeans',	
									'speciesRichness',	'resourcePotential',	'invCVBiomass',	'meanMaxAge',	
									'largeSpeciesIndicator','meanTrophicLevelCommunity','meanMaxL',
									'predatoryFish','biomassRatioInv2Dem','biomassRatioPel2Dem','meanTrophicLevelCommunity','ratioIndicator', 'biomassRetained','heips')) {
									#not length based indicators	
		
		#Get Data		
	if(biomass.data.from.qcorr & !qadjusted & !qadjPostStrat)  fna <- paste(path,"/data/stratified/notlengthbasedbiomassfromqcorrnotqadjpoststratnotqadj/",groups,"/",yrs[i],".RData",sep="")		
	if(biomass.data.from.qcorr &  qadjusted & !qadjPostStrat)  fna <- paste(path,"/data/stratified/notlengthbasedbiomassfromqcorrnotqadjpoststratqadj/",groups,"/",yrs[i],".RData",sep="")		
	if(biomass.data.from.qcorr & qadjusted & qadjPostStrat)    fna <- paste(path,"/data/stratified/notlengthbasedbiomassfromqcorrqadjpoststratqadj/",groups,"/",yrs[i],".RData",sep="")		

		load(fna)
		names(out) <- c('SPECIES','ID','BIOMASS','ABUNDANCE')
		out <- out[out$BIOMASS!=0,]	#remove the zeros as all species were assumed across all areas	
		}
		
		
		#data ready for indicator calculations
					if(indicator=='kemptonQ') 				 	outputs[[i]] <- kemptonQ(out, metric=ags[2]) #August 29, 2013 
					if(indicator=='margalef')                	outputs[[i]] <- margalef(out,group=ags[1], metric=ags[2]) #August 29, 2013 
					if(indicator=='shannon')                 	outputs[[i]] <- shannon(out,group=ags[1], metric=ags[2]) #August 29, 2013 
					if(indicator=='hillN1')                  	outputs[[i]] <- hillN1(out,group=ags[1], metric=ags[2]) #August 29, 2013 
					if(indicator=='hillN2')                  	outputs[[i]] <- hillN2(out,group=ags[1], metric=ags[2]) #August 29, 2013 
					if(indicator=='pielouSpeciesEvenness')   	outputs[[i]] <- pielouSpeciesEvenness(out,group=ags[1], metric=ags[2]) #August 29, 2013 
					if(indicator=='margalefSpeciesRichness') 	outputs[[i]] <- margalefSpeciesRichness( out,group=ags[1], metric=ags[2]) #August 29, 2013 
					if(indicator=='speciesRichness')         	outputs[[i]] <- speciesRichness(out,group=ags[1],grps=groups) #August 29, 2013 
					if(indicator=='resourcePotential' | 
								indicator=='invCVBiomass')		outputs[[i]] <- resourcePotential(out,group=ags[1], metric=ags[2],user.defined=ags[3],yr=yrs[i]) #August 29, 2013 
					if(indicator=='ratioIndicator')				outputs[[i]] <- ratioIndicator(out,group=c(ags[1],ags[4]),metric=ags[2],user.defined=ags[3]) #NEED TO CHECK
					if(indicator=='biomassRatioInv2Dem')		outputs[[i]] <- biomassRatioInv2Dem(out,syear=yrs[i]) #NEED TO CHECK
					if(indicator=='biomassRatioPel2Dem')		outputs[[i]] <- biomassRatioPel2Dem(out) #August 29, 2013 
					if(indicator=='meanMaxL')                	outputs[[i]] <- meanMaxL(out, metric=ags[2],path=path) #August 29, 2013 
					if(indicator=='meanMaxAge')              	outputs[[i]] <- meanMaxAge(out, metric=ags[2]) #August 29, 2013 
					if(indicator=='biomassPerTL')            	outputs[[i]] <- biomassPerTL(out, metric=ags[2],path=path) # August 29, 2013 
					if(indicator=='largeSpeciesIndicator')   	outputs[[i]] <- largeSpeciesIndicator(out)	#August 29, 2013 
					if(indicator=='predatoryFish')   		 	outputs[[i]] <- predatoryFish(out)	#August 29, 2013
					if(indicator=='meanTrophicLevelCommunity')  outputs[[i]] <- meanTrophicLevelCommunity(out)
					if(indicator=='biomassRetained') 			outputs[[i]] <- biomassRetained(out,path=path,metric=ags[2])
					if(indicator=='heips')   	outputs[[i]] <- heips(out,group=ags[1], metric=ags[2]) #August 29, 2013 
					

				
												
if(indicator %in% c('meanLengthCommunity','largeFishIndicator','communityFultonK','meanTrophicLevelCommunityLength')) {
			#requires length based data	
	if(biomass.data.from.qcorr & !qadjusted & !qadjPostStrat)  fna <- paste(path,"/data/stratified/lengthbasedbiomassfromqcorrnotqadjpoststratnotqadj/",groups,"/",yrs[i],".RData",sep="")		
	if(biomass.data.from.qcorr &  qadjusted & !qadjPostStrat)  fna <- paste(path,"/data/stratified/lengthbasedbiomassfromqcorrnotqadjpoststratqadj/",groups,"/",yrs[i],".RData",sep="")		
	if(biomass.data.from.qcorr & qadjusted & qadjPostStrat)    fna <- paste(path,"/data/stratified/lengthbasedbiomassfromqcorrqadjpoststratqadj/",groups,"/",yrs[i],".RData",sep="")		
	
			load(fna)
	
			inx <- out$FLEN==-99
			
		if(indicator=='meanTrophicLevelCommunityLength') {
			out[inx,'FLEN'] <- 10
		}  else {
			out <- out[!inx,]
		}
			if(any(out$FLEN==-99)) browser()
						
		if(indicator=='meanLengthCommunity')  						outputs[[i]] <- meanLengthCommunity(out,metric=ags[2])
		if(indicator=='largeFishIndicator')  						outputs[[i]] <- largeFishIndicator(out,metric=ags[2])
		if(indicator=='communityFultonK') 							outputs[[i]] <- communityFultonK(out,group=ags[1],gp=groups,yr=yrs[i],path=path)
		if(indicator=='meanTrophicLevelCommunityLength') 			outputs[[i]] <- meanTrophicLevelCommunityLength(out,metric=ags[2],path=path)

		}
				outputs[[i]]$YEAR <- rep(yrs[i],nrow(outputs[[i]]))
				

	}
	
	oo <- as.data.frame(do.call(rbind,outputs))
		if(indicator=='invCVBiomass') oo <- invCVBiomass(oo)  
}

if(substr(indicator,1,1) ==toupper(substr(indicator,1,1))) {
	if(groups == 'strat') saveIndicatorData <- FALSE; oo<- NA		
	if(groups != 'strat') {
	cat('Landings indicator',indicator,'\n')	 
			h=3
			load(paste(path,"/data/landings/landings.RData",sep=""))
			dat <- land
			rm(land)
			#grp <- sqlQuery(channel,paste('select * from landings_groupings;'))
			grp <- read.csv(file.path(path,"extra info","landingsgroupings.csv"))
			wl <- grp[,c(1,which(toupper(groups)== names(grp)))]
			names(wl) <- c('NAFO_UNIT','NAMES')
			dat <- merge(dat,wl,by='NAFO_UNIT')
			
		if(indicator=='IVILandings') 			out <- IVILandings(land=dat,path=path)
		if(indicator=='MeanTLLandings')			out <- MeanTLLandings(land=dat)
		if(indicator=='FishingInBalance')		out <- FishingInBalance(land=dat)
		if(indicator=='LandByGroup') 			out <- LandByGroup(land=dat,group=ags[1])
		if(indicator=='MarineTrophicIndex') 	out <- MeanTLLandings(land=dat,cutoff=3.25)
		if(indicator=='FishingPressure') 		out <- FishingPressure(path=path, land=dat,groups=groups, group=ags[1],qadj=qadjusted,qadjPostStrat=qadjPostStrat) 		
		if(indicator=='InverseFishingPressure')	out <- InverseFishingPressure(path=path, land=dat,groups=groups, group=ags[1],qadj=qadjusted,qadjPostStrat=qadjPostStrat) 		
		if(indicator=='LandingsDiversity') 		out <- LandingsDiversity(land=dat,grps=groups)
		oo <- out
		}
		}
	         
if(saveIndicatorData) {
		W <- 1
		if(qadjPostStrat) W <- 2
 		b<-Sys.time()
		b<-format(b,format="%m-%d",tz='America/Halifax')
		if(!qadjusted & !qadjPostStrat)         															fp <- file.path(path,"output","Estimated Indicators",b,"nonQadj","Output")
		if( qadjusted & !qadjPostStrat)                             										fp <- file.path(path,"output","Estimated Indicators",b,"Qadj","setlevel")
		if( qadjusted &  qadjPostStrat)                             										fp <- file.path(path,"output","Estimated Indicators",b,"Qadj","stratifiedLevel")
		if( substr(indicator,1,1) ==toupper(substr(indicator,1,1)) & !indicator %in% 
						c('InverseFishingPressure','FishingPressure')) 										fp <- file.path(path,"output","Estimated Indicators",b,"Land","Output")
		if(indicator %in% c('InverseFishingPressure','FishingPressure') & qadjusted==F)    				fp <- file.path(path,"output","Estimated Indicators",b,"nonQadj","Output")      
		if(indicator %in% c('InverseFishingPressure','FishingPressure') & qadjusted==T & qadjPostStrat)   fp <- file.path(path,"output","Estimated Indicators",b,"Qadj","setlevel")       
		if(indicator %in% c('InverseFishingPressure','FishingPressure') & qadjusted==T & !qadjPostStrat)	fp <- file.path(path,"output","Estimated Indicators",b,"Qadj","stratifiedLevel")
		
		dir.create(fp,showWarnings=F,recursive=T)
		f<-file.path(fp,paste(indicator,groups,ags[1],ags[2],".csv",sep=""))
		write.csv(oo,f)
  		}
		return(oo)
}	
		
