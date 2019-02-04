#--Indicators for spera project October 31, 2012 02:25:51 PM 
#
#
 
#biomass q adju using medians from harley and myers tech report
#need to ensure 4X herring is not double q corrected 
qBiomassPostStrat<-function(aa,path=path) {

		qa <- read.csv(file.path(path,"extra info","qcorrinfo.csv"))
		
		names(qa)[1] <- 'SPECIES'
		ab <- merge(aa,qa,by='SPECIES',all.x=T)
		
	for(i in 1:nrow(ab)) {
	 	v <- ab[i,]
	 	if(is.na(v$fungroup) & is.na(v$q)) v$q <- 1
		#adjust the catch at lengths by appropriate q and length correction
		#single q for species


	 	if(is.na(v$fungroup) & !is.na(v$q) ) {
		 							q <- v$q
		 							} 
	 	
		 if(is.na(v$fungroup) & is.na(v$q)) {
		 							q <- 1
		 							} 							
		 
 		#cod length based q
	 	else if(v$fungroup=='cod' & (v$q==0 | is.na(v$q)) ) 
	 								{a1=-5.14
	 					 			b1=0.141
	 					 			g1=0.870
	 					 			q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))		 						
	 								}
	 	#haddock length based v$q
	 	else if (v$fungroup=='haddock'& v$q==0 ) 
	 							{	a1=-2.80
	 							  	b1=0.0661
	 					 			g1=1.5
	 					 			q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))
		 						}
	 	
	 	#pelagic gadoids length based v$q
	 	else if(v$fungroup=='pg'& v$q==0 )
	 	    					   {a1=-4.61
	 					 			b1=0.0789
	 					 			g1=0.58
	 					 			q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))
		 							}
		
		#demersal gadoids length based v$q
		else if(v$fungroup=='dg'& v$q==0)
									{a1=-3.50
									 b1=0.0925
									 g1=0.968	
									 q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))
									}
		
	 	#flat fish length based v$q
		else if(v$fungroup=='ff'& v$q==0)
									{a1=-4.35
									 b1=0.111
									 g1=0.831	
									 q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))
									}
		
	 	# ling length based v$q	 	
	 	else if(v$fungroup=='ling' & v$q==0) 
	 							  {a1=-13.9
									 b1=0.193
									 g1=1.66	
									 q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))
								}
		# small pelagics length based v$q from H.Benoit
		else if(v$fungroup=='sp' & v$q==0) 
	 							{a1=-17.7165
									 b1=0.175
									 g1=37710.7	
									 q<-g1*(exp(a1+b1*(v$FLEN*v$lencorr)))/(1+exp(a1+b1*(v$FLEN*v$lencorr)))
								}

		 							v$BIOMASS<-v$BIOMASS/q
		 							v$ABUNDANCE<-v$ABUNDANCE/q
		 						ab[i,] <- v
				}

		aa <- ab[,-which(names(ab) %in% c('fungroup', 'q', 'lencorr'))]								
	return(aa)
		}

