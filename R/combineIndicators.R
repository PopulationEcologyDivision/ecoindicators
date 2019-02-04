#path <- 'F:/Indicators'

combineIndicators <-	function(path,ind,stratifiedQ=T) {
		fp 		<- file.path(path,"output","Estimated Indicators")
 		fi 		<- dir(fp,full.names=T,recursive=T)
 		nc 		<- length(strsplit(fi[1],"/")[[1]])
 		
 		o 		<- matrix(NA,ncol=nc,nrow=length(fi))
 	for(i in 1:length(fi)) {
 		o[i,] 	<- strsplit(fi[i],"/")[[1]]
 		}
 		
 		
 		if(any(duplicated(o[,(nc-1):nc]))) {
 			k 	<- bothDuplicated(o[,(nc-1):nc])
 			cat('You have duplicated files:\n remove before proceeding\n')
 			print(fi[k])
 			stop()
 		}
 	
 		#setup indices
 		if(stratifiedQ)  sq <- which(o[,nc-1] %in% c('Output','stratifiedLevel'))
 		if(!stratifiedQ) sq <- which(o[,nc-1] %in% c('Output','setlevel'))

 		
 		#
 		qa 		<- which(o[,nc-2]=='Qadj')
 		nqa 	<- which(o[,nc-2]=='nonQadj')
 		la 		<- which(o[,nc-2]=='Land')
 		
 		
 		ew 		<- grep(ind,o[,nc])
 		

 		ewq 	<- fi[intersect(ew,intersect(sq,qa))]
 		ewnq 	<- fi[intersect(ew,intersect(sq,nqa))]
 		ewla  	<- fi[intersect(ew,la)]
 		

 		#     
 		csv.ewq    	 <- combineFiles( ewq  )
 		csv.ewnq     <- combineFiles( ewnq  )
 		
 		if(length(ewla)>0) {
 			csv.ewla    <- combineFiles( ewla  )
        	csv.ewq      <- merge(csv.ewq    ,csv.ewla ,by=c('YEAR','ID'),all.x=T)
			csv.ewnq	 <- merge(csv.ewnq   ,csv.ewla ,by=c('YEAR','ID'),all.x=T)
		}
				
		names(csv.ewq   ) 	<- rename(path,ind=ind,  oldnames=names(csv.ewq   ))
		names(csv.ewnq   )  <- rename(path, oldnames= names(csv.ewnq	 ),ind=ind)
		
		
    if(any(names(csv.ewq)=='InverseFPInvertebrates.L')) {    
		  csv.ewq$InverseFPInvertebrates.L[csv.ewq$InverseFPInvertebrates.L==0] <- NA
		  csv.ewnq$InverseFPInvertebrates.L[csv.ewnq$InverseFPInvertebrates.L==0] <- NA
		}
		
    if(any(names(csv.ewq)=='FPInvertebrates.L')) {  	
		  csv.ewq$FPInvertebrates.L[is.infinite(csv.ewq$FPInvertebrates.L)] <- NA
		  csv.ewnq$FPInvertebrates.L[is.infinite(csv.ewnq$FPInvertebrates.L)] <- NA
		}
	
		
		if(any(names(csv.ewq)=='BiomassInvertebrates')) {
		  csv.ewq$BiomassInvertebrates[which(csv.ewq$YEAR <1999)] <- NA
		  csv.ewnq$BiomassInvertebrates[which(csv.ewnq$YEAR <1999)] <- NA
		  
		}
		
#   September 29 2016  		
#   Two indicators (RV survey) have zeros (at the strata level mostly): LargeFishIndicator & LargeSpeciesIndicator. 
#   These zeros are changed to NAs below (NA’s were considered real NA’s since even if not captured in the survey, they would still be present, just not detected or perhaps not recorded. If we treated them as zeros, this would have an impact on trends, whereas treating them as NAs does not) 
    if(any(names(csv.ewq)=='LargeSpeciesIndicator')) {  	
		  csv.ewq$LargeSpeciesIndicator[csv.ewq$LargeSpeciesIndicator==0] <- NA
		  csv.ewnq$LargeSpeciesIndicator[csv.ewnq$LargeSpeciesIndicator==0] <- NA
		}
    
		if(any(names(csv.ewq)=='LargeFishIndicator')) {  	
		  csv.ewq$LargeFishIndicator[csv.ewq$LargeFishIndicator==0] <- NA
		  csv.ewnq$LargeFishIndicator[csv.ewnq$LargeFishIndicator==0] <- NA
		}
    
#   September 27 2016 - the following lines were created to set to NA all indi at the strata scale that had more than 25% of NA's
#   Note: We did not use this as Bundy decided to set NA's after csv files are produced outside this function
#   if(ind == 'strat') csv.ewq = filterIndicators(x = csv.ewq, path=NULL,file.name=NULL,c('CCLargeBenthivore','BInvertebrateToDemersal','BTGLargeBenthivore','BiomassTL2','BiomassClupeids','CCPlanktivore'))
#	  if(ind =='strat') csv.ewnq = filterIndicators(x = csv.ewnq, path=NULL,file.name=NULL,c('CCLargeBenthivore','BInvertebrateToDemersal','BTGLargeBenthivore','BiomassTL2','BiomassClupeids','CCPlanktivore', 'BPelagicToDemersal'))
	
#   September 29 2016
#   Some landings indicators (.L) had NAs (when merging tables across all years, if some of the tables have missing years they get filled with NA for that missing year). 
#   All landings indicators (.L) with missing years should be zero, not NA. Thus, NAs were replaced with zeros since no data = no landings. 
#   This was applied to the following indicators at the following spatial scales: 
#   LClupeids.L; LForageFish.L; LInvertebrates.L; LLargePelagic.L; LSkates.L. shelfsetq, esswsssetq, nafosetq.
#   This change was done using combineIndicators.r script (via filterNA2Zero.r function).

		if(ind != 'strat') csv.ewq = filterNA2Zero(x =csv.ewq, path=NULL,file.name=NULL, c('LClupeids.L','LForageFish.L','LInvertebrates.L','LLargePelagic.L','LSkates.L'))
		if(ind !='strat') csv.ewnq = filterNA2Zero(x =csv.ewnq, path=NULL,file.name=NULL, c('LClupeids.L','LForageFish.L','LInvertebrates.L','LLargePelagic.L','LSkates.L'))

		# #  September 29 2016
		# #  Note: In the latest extraction of indicators (Gomez, September 28 2016) landings up to 2015 were not extracted (even though years for all landings scripts are updated up to 2015). 
		# #  Consequently, for now, NA’s for landings indicators in 2015 will remain as NA’s: 		
		# # October 19 2016: Landings indicators extracted up to 2016 thus we commented the lines below
		# if(any(names(csv.ewq)=='LClupeids.L')) {
		#   csv.ewq$LClupeids.L[which(csv.ewq$YEAR >2014)] <- NA
		#   csv.ewnq$LClupeids.L[which(csv.ewnq$YEAR >2014)] <- NA
		# }
		# 
		# if(any(names(csv.ewq)=='LForageFish.L')) {
		#   csv.ewq$LForageFish.L[which(csv.ewq$YEAR >2014)] <- NA
		#   csv.ewnq$LForageFish.L[which(csv.ewnq$YEAR >2014)] <- NA
		# }
		# 
		# if(any(names(csv.ewq)=='LInvertebrates.L')) {
		#   csv.ewq$LInvertebrates.L[which(csv.ewq$YEAR >2014)] <- NA
		#   csv.ewnq$LInvertebrates.L[which(csv.ewnq$YEAR >2014)] <- NA
		# }
		# 
		# if(any(names(csv.ewq)=='LLargePelagic.L')) {
		#   csv.ewq$LLargePelagic.L[which(csv.ewq$YEAR >2014)] <- NA
		#   csv.ewnq$LLargePelagic.L[which(csv.ewnq$YEAR >2014)] <- NA
		# }
		# 
		# if(any(names(csv.ewq)=='LSkates.L')) {
		#   csv.ewq$LSkates.L[which(csv.ewq$YEAR >2014)] <- NA
		#   csv.ewnq$LSkates.L[which(csv.ewnq$YEAR >2014)] <- NA
		# }
		# 

#   September 30 2016 
#		Some fishing pressure (.L) indicators had NA’s. If landings indicators 
#		equal to zero, then NAs of Fishing Pressure were set to zero as well. 
#		The remaining NA’s in FP will remain as NA’s because there are landings, 
#		but not estimate of biomass, which means there is a value for FP, its just not available
		
		if(any(names(csv.ewq)=='FPClupeids.L')) {
		  csv.ewq$FPClupeids.L[which(csv.ewq$LClupeids.L == 0)] <- 0
		  csv.ewnq$FPClupeids.L[which(csv.ewnq$LClupeids.L == 0)] <- 0
		}
		

    if(any(names(csv.ewq)=='InverseFPClupeids.L')) {
      csv.ewq$InverseFPClupeids.L[which(csv.ewq$LClupeids.L == 0)] <- 0
      csv.ewnq$InverseFPClupeids.L[which(csv.ewnq$LClupeids.L == 0)] <- 0
    }

		
		if(any(names(csv.ewq)=='FPForageFish.L')) {
		  csv.ewq$FPForageFish.L[which(csv.ewq$LForageFish.L == 0)] <- 0
		  csv.ewnq$FPForageFish.L[which(csv.ewnq$LForageFish.L == 0)] <- 0
		}
		

    if(any(names(csv.ewq)=='InverseFPForageFish.L')) {
      csv.ewq$InverseFPForageFish.L[which(csv.ewq$LForageFish.L == 0)] <- 0
      csv.ewnq$InverseFPForageFish.L[which(csv.ewnq$LForageFish.L == 0)] <- 0
    }
		
		if(any(names(csv.ewq)=='FPInvertebrates.L')) {
		  csv.ewq$FPInvertebrates.L[which(csv.ewq$LInvertebrates.L == 0)] <- 0
		  csv.ewnq$FPInvertebrates.L[which(csv.ewnq$LInvertebrates.L == 0)] <- 0
		}
		
    if(any(names(csv.ewq)=='InverseFPInvertebrates.L')) {
      csv.ewq$InverseFPInvertebrates.L[which(csv.ewq$LInvertebrates.L == 0)] <- 0
      csv.ewnq$InverseFPInvertebrates.L[which(csv.ewnq$LInvertebrates.L == 0)] <- 0
    }
		
		if(any(names(csv.ewq)=='FPSkates.L')) {
		  csv.ewq$FPSkates.L[which(csv.ewq$LSkates.L == 0)] <- 0
		  csv.ewnq$FPSkates.L[which(csv.ewnq$LSkates.L == 0)] <- 0
		}
		
    if(any(names(csv.ewq)=='InverseFPSkates.L')) {
      csv.ewq$InverseFPSkates.L[which(csv.ewq$LSkates.L == 0)] <- 0
      csv.ewnq$InverseFPSkates.L[which(csv.ewnq$LSkates.L == 0)] <- 0
    }
		
# S T A N D A R D I Z E   D A T A 

		csv.ewq    <- stdizeFrame( csv.ewq   )
		csv.ewnq   <- stdizeFrame( csv.ewnq  )
		
		if(any(names(csv.ewq)=='InverseCVBiomass')) {		
		  csv.ewq$InverseCVBiomass[csv.ewq$InverseCVBiomass==0] <- NA
		  csv.ewnq$InverseCVBiomass[csv.ewnq$InverseCVBiomass==0] <- NA
		}

    csv.ewq <- do.call(data.frame,lapply(csv.ewq, function(x) replace(x, is.infinite(x),NA)))
		csv.ewnq <- do.call(data.frame,lapply(csv.ewnq, function(x) replace(x, is.infinite(x),NA)))

	da<-c("csv.ewq", "csv.ewnq")
		for(i in 1:length(da)) {
		writeFiles(path,x=get(da[i]),na=da[i],ind=ind,stratifiedQ=stratifiedQ)
  }
        
        }