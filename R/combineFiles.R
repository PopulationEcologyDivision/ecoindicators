    combineFiles <- function(x) {
     	for(i in 1:length(x)) {
     	 ou <- read.csv(x[i],header=T)
     	 ou <- ou[,-1]
     	 if(any(names(ou) %in% c('NAMES','IDS','uI'))) names(ou)[which(names(ou) %in% c('NAMES','IDS','uI'))] <- "ID"
     	 if(!grepl('biomassPerTL',x[i])) ih <- which(!names(ou) %in% c('ID','YEAR'))
     	 if(grepl('biomassPerTL',x[i])) ih <- which(!names(ou) %in% c('ID','YEAR','TL'))
     	 #if(grepl('biomassRatioPel2Dem',x[i])) ou <- ou[,!names(ou) %in% c('BIOMASS.x','BIOMASS.y')]
     	 if(grepl('nafo',x[i]))    nn <-  sub('.csv','',sub('nafo','',x[i]))
     	 if(grepl('strat',x[i]))   nn <-  sub('.csv','',sub('strata','',x[i]))
     	 if(grepl('esswss',x[i]))     nn <-  sub('.csv','',sub('esswss','',x[i]))
     	 if(grepl('shelf',x[i]))   nn <-  sub('.csv','',sub('shelf','',x[i]))
     	 if(grepl('indiseas',x[i]))   nn <-  sub('.csv','',sub('indiseas','',x[i]))

     	 nc <- length(strsplit(nn,"/")[[1]])	 
     	 nn <- strsplit(nn,"/")[[1]][nc]
     	 names(ou)[ih] <- nn
     	 
     	  if(grepl('biomassPerTL',x[i])) {
     	  ou <- reshape(ou,timevar='TL',idvar=c('ID','YEAR'),direction='wide')
     	    }
     if(i==1) {
     	 	out <- ou
     	 	} else {
     	 	if(any(!c('YEAR','ID') %in% names(out)) | any(!c('YEAR','ID') %in% names(ou))) browser()
     	 	out <- merge(out,ou,by=c('YEAR','ID'),all=T)
     	 	}
     	 	}
     	 out <- out[!is.na(out$ID),]
     	 return(out)
     	 }
    
    
 