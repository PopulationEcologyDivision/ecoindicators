getLandings <- function(path) {


	NAFO <- function(area=paste("4VS","4VN","4X","4W",sep="','")) {
			y <- 1968:1985
			out <- list()
		channel.ca<-odbcConnect("ptran",uid="cooka",pwd="bzz7plf")

		for( i in 1:length(y)) {
			#if(y[i]==1975) {
			#change April 17, 2013 Funny landings stuff with subtrips in some years--changed to summary as we are not interested in subunits now
			#fixed a join where one to many species names which was mucking things up
			out[[i]] <- sqlQuery(channel.ca,paste("select ",y[i]," year,description nafo_unit,species,sum(catch) catch 
													from comland.nafo_summary a,comland.nafo_area_codes r
													where a.area=r.area and upper(description) in ('",area,"') and year_of_activity = ",y[i],"
													group by ",y[i],",description, species",sep=""))
													}				
					
			#out[[i]]<- sqlQuery(channel.ca,paste("select year,nafo_unit,marfis2allcodes species,sum(wt) catch 
			#										from (select ",y[i]," year,b.id, nafod,unit,nafo_unit,species_code,wt 
			#										from (select catchers_recid||','||region_code||','||trip_num||','||sub_trip_num id,
			#												year_of_activity,nafo_division_code nafod ,nafo_unit_area unit,nafo_division_code||nafo_unit_area nafo_unit
			#													from comland.sub_trips_",y[i]," where nafo_division_code in ('",area,"')) a,
			#											(select catchers_recid||','||region_code||','||trip_num||','||sub_trip_num id, species_code, sum(live_wt) wt
			#												from comland.identified_catches_",y[i]," 
			#															group by catchers_recid||','||region_code||','||trip_num||','||sub_trip_num, species_code) b
			#											where b.id=a.id) a, 
			#										mfd_stomach.indiseas_marfis2nafo@stomach.quoddy3 b, mfd_stomach.indiseas_marfis2allcodes@stomach.quoddy3 c 
			#										where 
			#										a.species_code=b.marfis_code and b.marfis_code=c.MARFIS 
			#										group by year,nafo_unit, marfis2allcodes",sep=""))
			#										}
			dat <- do.call(rbind,out)

			return(dat)		
		}
	
	
	#ZO  - 1986:2002
		
ZIF <- function (area=paste("4VS","4VN","4X","4W",sep="','")) {
	#match the zif data dictionary landings 
		channel.ca<-odbcConnect("ptran",uid="cooka",pwd="bzz7plf")

			y <- 1986:2002
			out <- list()
			for( i in 1:length(y)) {
			out[[i]]<-sqlQuery(channel.ca,paste("select year,nafo_unit,zif2allcodes species,sum(wt) catch 
													from (select ",y[i]," year,b.id, nafod,unit,nafo_unit,species_code,wt 
													from (select catchers_recid||','||region_code||','||trip_num||','||sub_trip_num id,
															year_of_activity,nafo_division_code nafod ,nafo_unit_area unit,nafo_division_code||nafo_unit_area nafo_unit
																from cl.sub_trips_",y[i]," where nafo_division_code in ('",area,"')) a,
														(select catchers_recid||','||region_code||','||trip_num||','||sub_trip_num id, species_code, sum(live_wt) wt
															from cl.identified_catches_",y[i]," 
																		group by catchers_recid||','||region_code||','||trip_num||','||sub_trip_num, species_code) b
														where b.id=a.id) a, 
													mfd_stomach.indiseas_zif2allcodes b
													where 
													a.species_code=b.zif  
													group by year,nafo_unit, zif2allcodes",sep=""))
													}
			dat <- do.call(rbind,out)
					return(dat)		
				}
				
				
MARFIS <- function(area=paste("4V","4X","4W",sep="','")) {
	#match the vdc marfis landings
channel.ca<-odbcConnect("ptran",uid="cooka",pwd="bzz7plf")
	data <- sqlQuery(channel.ca,paste("select year_fished year,unit_area nafo_unit, marfis2allcodes species,sum(round(rnd_weight_kgs/1000,4)) catch
	     						from mfd_obfmi.marfis_catch_effort d, mfd_stomach.indiseas_marfis2allcodes a
									where upper(nafo_div) in ('",area,"') and d.species_code=a.marfis
	          						and year_fished between '2003' and '2016'
	          						and category_desc not in 'OTHER'
	         			GROUP BY year_fished ,unit_area, marfis2allcodes
	         	       ;",sep=""))
	         	      odbcClose(channel.ca)

	       return(data)
}
print('Make sure you change the year in this function or you will only get up to 2016')
       
ndat <- NAFO()
zdat <- ZIF()
mdat <- MARFIS()

channel <- odbcConnect('ptran','mfd_stomach','bx547mj9')

nam <- sqlQuery(channel,paste("select * from mfd_stomach.indiseas_allcodes;"))
		names(nam)[1] <-'SPECIES'
		dat <- rbind(ndat,zdat,mdat)
		dat<-dat[with(dat,order(SPECIES,YEAR)),]
		dat<- dat[!dat$SPECIES %in% c(164,368,399,400,420,422,460,502,542,589,623,889,900,901,902,905,906,908,922,923,927,956,199),] #species with very sparse info and small landings
		dat$SPECIES <- ifelse(dat$SPECIES %in% c(512,514,516,518,520,525,529),529,dat$SPECIES) #combine the clams
		dat$SPECIES <- ifelse(dat$SPECIES %in% c(562,564),564,dat$SPECIES) #combined the whelks and periwinkles
		land<- merge(nam,dat)
		land$NAFO_UNIT <- toupper(land$NAFO_UNIT)
		land[land$NAFO_UNIT=='4VSA','NAFO_UNIT'] <- '4VSB'
		land[land$NAFO_UNIT=='4VSS','NAFO_UNIT'] <- '4VSU'
		land[land$NAFO_UNIT=='4VNN','NAFO_UNIT'] <- '4VN'
		fp = file.path(path,'data','landings')
		dir.create(fp, recursive=T, showWarnings=F)
		
		save(land,file=file.path(fp,"landings.RData"))
		return(land)
}   
   
path
