resourcePotential <- function(X,user.defined=F, group=c('FINFISH','ALL','SKATES','CLUPEIDS','GROUNDFISH','FLATFISH','GADOIDS','INVERTEBRATES','FORAGE','PELAGIC','LBENTHIVORE','MBENTHIVORE','PISCIVORE',  
														'PLANKTIVORE','ZOOPISCIVORE'),metric=c('BIOMASS','ABUNDANCE'),herring=T,yr=0)  {
														Y<-FALSE
			#IF USER DEFINED FOR GROUP PUT IN A VECTOR OF THE SPECIES CODES YOU WANT TO INCLUDE
			if(user.defined) {
			Y <- X[X$SPECIES %in% group,]	
			
			} else {
				if(group=='ALL') 			Y <- X
				if(group=='FINFISH') 		Y <- X[X$SPECIES %in% c(1:1000),] 
				if(group=='SKATES')     	Y <- X[X$SPECIES %in% c(200,201,202,203,204,205,206,207,208,209,210,211),]
				if(group=='CLUPEIDS')		Y <- X[X$SPECIES %in% c(60,61,62,63),]
				#if(group=='GROUNDFISH' )  Y <- X[X$SPECIES %in% c(10:22,24:59,140,141,142,143,110,111,112,113,114,115,116,117,118,200,201,202,203,204,205,206,207,208,209,210,
				#									211,220,221,300,301,304,310,320,340,350,400,620:650),]    	
				#SOTO EXTRACTION
				if(group=='GROUNDFISH' )  Y <- X[X$SPECIES %in% c(10,11,12,13,14,15,16,17,18,19,20,21,25,28,30,31,35,40,41,42,43,44,49,50,51,52,59,140,141,142,143,110,111,112,114,115,117,118,200,201,202,203,204,211,221,300,301,304,310,320,340,350,400,620,650,122,123,142,143,149,156,216,302,303,306,307,313,314,316,331,341,410,411,412,414,501,503,505,508,512,520,595,602,603,604,616,617,619,620,621,622,623,624,625,626,628,630,631,632,633,637,640,641,647,704,714,742,743,744,816,880),]  
				if(group=='FLATFISH')     	Y <- X[X$SPECIES %in% c(30,31,40,41,42,43,44,45,49,140,141,142,143),]
				if(group=='GADOIDS')     	Y <- X[X$SPECIES %in% c(10,11,12,13,14,15,16,17,18,19,110,111,112,113,114,115,116,117,118),]
				if(group=='INVERTEBRATES' & yr>=1999)  Y <- X[X$SPECIES %in% c(2000:8999),]
				if(group=='FORAGE')     	Y <- X[X$SPECIES %in% c(60,61,62,63,64,610,160),]
				if(group=='PELAGIC')     	Y <- X[X$SPECIES %in% c(60,61,62,63,64,610,160,70),]
				if(group=='LBENTHIVORE')    Y <- X[X$SPECIES %in% c(50,200),]
				if(group=='MBENTHIVORE')    Y <- X[X$SPECIES %in% c(11,241,	40,	41,	42,	43,	123,	142,	143,	202,	203,	301,	304,	414,	501,	505,	640,	114,	115),]
				if(group=='PISCIVORE')    	Y <- X[X$SPECIES %in% c(10,	12,	15,	16,	30,	31,	112,	201,	204,	220,	300,	320,	400),]
				if(group=='PLANKTIVORE')    Y <- X[X$SPECIES %in% c(60,61,62,70,160,610,701,64),]
				if(group=='ZOOPISCIVORE')    Y <- X[X$SPECIES %in% c(13,14,19,23),]
				if(herring==FALSE) 			Y <- Y[Y$SPECIES !=60,]

			}
			if(nrow(Y)==0 || Y==FALSE ) dat<- data.frame(ID=unique(X$ID),Y=0)
			else {
			dat <- aggregate(Y[metric],by=Y['ID'],FUN=sum)
			}
			names(dat) <- c('ID','BIOMASS')
						return(dat)
		}	
			
