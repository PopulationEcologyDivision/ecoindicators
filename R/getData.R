#get the data for indicators

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#NOTES to REMEMBER
#November 02, 2012 11:57:17 AM  Alida Bundy and Adam COok decided to use Julio's TL levels for this work as they are most current and based on better diet data
#for species with 0 abundance or zero biomass I estimated the total weight/abudance for all the non zero sets across the full survey estimate abundance where weight or abund data was available
#MAXIMUM LENGTH TAKEN AS THE LENGTH AT 99% OF MAX OBSERVED from all time series of data November 06, 2012 11:14:05 AM 
#using ecopath data from julio for trophic levels and ee and whatnot
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

 if(!any(ls()=='channel')) source('R:/Shared/ACook/index.txt')
###--------------------------------------------------------------------------------------------------------						        
######Extract the biodiversity data for use in indicators 1,3,4,5,8,9,10,11,12-22,24
	

##------
###create table to fill in missing wts or nos

#wt <- sqlQuery(channel,paste("select spec,round(avg(wts),3) mean_wt_fish from (select spec,totwgt/totno wts from groundfish.gscat where totno >0 and totwgt >0 and spec in 
#(10,	11,	12,	13,	14,	15,	16,	23,	30,	40,	41,	42,	43,	50,	60,	62,	90,	150,	160,	200,	201,	202,	203,	220,	241,	300,	304,	306,	320,	340,	501,	503,	512,	610,	611,	622,	623,	640,	642,	708,	712,	816,	1091,	2100,	2210,	2313,	2510,	2511,	2513,	2519,	2520,	2522,	2523,	2525,	2526,	2528,	2532,	2550,	2600,	4320,	4321,	4511,	4514,	4521,	6115,	6120,	6125,	6201,	8326
#)) group by spec;"))
#sqlSave(channel,wt,tablename='MEAN_WTS_FOR_FILL_IN',rownames=F)
#had to change 2522, 4320 and  2210 as all were >100kg per fish



##-----						        
#make a dataframe of these estimates October 23, 2012 12:09:00 PM done and saved as mfd_stomach.finfish_ab
calcAB <- function(species,s.year,e.year,s.strat,e.strat) {
	bb<-sqlQuery(channel,paste("
					 			SELECT
					 				",species,",flen,fwt 
					 			FROM
					 				groundfish.gsdet d, groundfish.gsinf i 
					 			WHERE
					 				i.mission=d.mission and i.setno=d.setno 
					 				and i.strat between '",s.strat,"' and '",e.strat,"' and to_char(sdate,'mm') in ('06','07','08') 
					 				AND to_char(sdate,'yyyy') between ",s.year," and ",e.year," 
	 								and spec in (",species,") and fwt is not null;",sep=""))
 				yr1=s.year;yr2=s.year
	 		 			if(nrow(bb)<100) {
	 		 				bb<-sqlQuery(channel,paste("
					 			SELECT
					 				",species,",flen,fwt 
					 			FROM
					 				groundfish.gsdet d, groundfish.gsinf i 
					 			WHERE
					 				i.mission=d.mission and i.setno=d.setno 
					 				and i.strat between '",s.strat,"' and '",e.strat,"' and to_char(sdate,'mm') in ('06','07','08') 
					 				AND to_char(sdate,'yyyy') between ",s.year,"-5 and ",e.year,"+5 
	 								and spec in (",species,") and fwt is not null;",sep=""))
	 								yr1=s.year-5;yr2=s.year+5
 									}				
	 					if(nrow(bb)<100){
	 					bb<-sqlQuery(channel,paste("
					 			SELECT
					 				",species,",flen,fwt 
					 			FROM
					 				groundfish.gsdet d, groundfish.gsinf i 
					 			WHERE
					 				i.mission=d.mission and i.setno=d.setno 
					 				and i.strat between '",s.strat,"' and '",e.strat,"' and to_char(sdate,'mm') in ('06','07','08') 
					 				and spec in (",species,") and fwt is not null;",sep=""))
					 			yr1=1970;yr2=2012
	 								}
	 	out<-with(bb,lm(log(FWT)~log(FLEN),bb))
	 	a.est<-coef(out)[1]
	 	b.est<-coef(out)[2]
	 	return(c(species,round(a.est,4),round(b.est,4),yr1,yr2, s.strat,e.strat))
	 	}
        

#-------
##getting the maxlength data

#	dd <- sqlQuery(channel,paste("select spec,flen,sum(clen) n from groundfish.gsdet where spec<1000 group by spec,flen;"))
#	sp <-	unique(dd$SPEC)
#	
#		
# 	calcMaxLen <- function(X,prop=0.99) {
# 		#X is a length frequency of data
# 		a<-cumsum(table(X)/length(X))
# 		out <- as.numeric(names(a[which.min(abs(a-prop))]))
# 		return(out)
# 		}
#	
#	out1<-list()
#	m=0
#	for(i in 1:length(sp)) {
#			tr <- dd[dd$SPEC==sp[i],]
#			g <- rep(tr$FLEN,tr$N)
#			if(length(g)>100) {
#			m=m+1
#			h <- calcMaxLen(g)
#	  		out1[[m]] <- c(sp[i],h)
#		}
#	}
#	out <- as.data.frame(do.call('rbind',out1))
#	out <- out[order(out[,1]),]
#	sqlSave(channel,out,tablename='INDISEAS_MAX_LENGTH',rownames=F)
#



































