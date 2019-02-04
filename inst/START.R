# -------- # Section 0. Make sure landings data is updated from MARFIS and replace data for lobster   #  ####

# path <- file.path('C:/RProjects/ExtractIndicators')
# require(RODBC)
# source(paste(path,'/R/amc helpers.R',sep=""))
# channel.ca<-odbcConnect("ptran",uid="",pwd="")
# source('C:/RProjects/ExtractIndicators/R/getLandings.R') 
# getLandings(path = "C:/RProjects/ExtractIndicators/") # this will get landings data updated in data/landings/landings.RData
# source('C:/RProjects/ExtractIndicators/R/lobster.R') #this will correct lobster data (March 2017)

# -------- # Section 1. Set path   #  ####
setwd("C:/RProjects/ExtractIndicators/")
path <- file.path('C:/RProjects/ExtractIndicators')
require(RODBC)
channel <- odbcConnect("***", uid="***", pwd="***")
source(paste(path,'/R/amc helpers.R',sep=""))

# -------- #Section 2. Extract all indicators individually #  ####
#Note: #All indicators are saved individually as csv files in: ExtractIndicators/output/EstimatedIndicators   
#source(paste(path,'/src/.START.R',sep=""))
loadfun(path)
#if biomass data needs to be updated run 
biomass.redo=F
path=file.path(path)
	if(biomass.redo)  {
	#November 12, 2013 last update
	biomassData(p=file.path(path),s.year=2015,e.year=2015)
		}

stratify.redo=F ## this gets all the biomass and abundance data by set level
	if(stratify.redo) {
		stratifyBiomass(p=path,s.year=1970,e.year=2015,qadjusted=T, biomass.data.from.qcorr=T, qadjPostStrat=F,lengthBased=F)
		stratifyBiomass(p=path,s.year=1970,e.year=2015,qadjusted=F, biomass.data.from.qcorr=T, qadjPostStrat=F,lengthBased=F)
		stratifyBiomass(p=path,s.year=1970,e.year=2015,qadjusted=T, biomass.data.from.qcorr=T, qadjPostStrat=F,lengthBased=T)
		stratifyBiomass(p=path,s.year=1970,e.year=2015,qadjusted=F, biomass.data.from.qcorr=T, qadjPostStrat=F,lengthBased=T)
		stratifyBiomass(p=path,s.year=1970,e.year=2015,qadjusted=T, biomass.data.from.qcorr=T, qadjPostStrat=T,lengthBased=F)
		stratifyBiomass(p=path,s.year=1970,e.year=2015,qadjusted=T, biomass.data.from.qcorr=T, qadjPostStrat=T,lengthBased=T)
		}	

#if indicators need to be redone enmasse - 

		indicator.redo=T
		start.i=1
		rm(end.i)
		if(indicator.redo) {
		print('Here we go.....')
			ex <- read.csv(file.path(path,"extra info","extraction infoOct22.csv")) # extraction infoOct22.csv has the new names for the indicators 
			ex <- ex[ex$rerun==1,]
			if(!exists('end.i')) end.i <- nrow(ex)
			areas <- c('strat','nafo','esswss','shelf')
			for(i in start.i:end.i) {
					for(j in 1:length(areas)) {
						if(sum(ex[i,4:5])==2) { #do both q and non q
						indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=T,biomass.data.from.qcorr=T,qadjPostStrat=T)
						indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=F,biomass.data.from.qcorr=T,qadjPostStrat=F)
						indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=T,biomass.data.from.qcorr=T,qadjPostStrat=F)
						} 
						if(sum(ex[i,4:5])==1) {
						if(ex[i,4]==1)	indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=T,biomass.data.from.qcorr=T,qadjPostStrat=F)
						if(ex[i,5]==1)	indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=F,biomass.data.from.qcorr=T,qadjPostStrat=F)
						if(ex[i,4]==1)	indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=F,biomass.data.from.qcorr=T,qadjPostStrat=T)
						}
						if(sum(ex[i,4:5])==0) {
		
						indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=F,biomass.data.from.qcorr=T,qadjPostStrat=F)
						
						}	
					}
				}	
			}
	
# -------- #Section 3. Use this if only single indicators need to be redone #  ####

#SOTO demersal indicator
		a <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c('GROUNDFISH','BIOMASS',user.defined=F),groups='nafo',qadjusted=F,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=F,yrs=1970:2015)
		b <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c('GROUNDFISH','BIOMASS',user.defined=F),groups='shelf',qadjusted=F,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=F,yrs=1970:2015)
		head(a)
	#indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c('PELAGIC','BIOMASS',user.defined=F),groups='indiseas',qadjusted=F,biomass.data.from.qcorr=T,saveIndicatorData=F,qadjPostStrat=F,yrs=1970:2013)
	#indicatorSpaceTime(path=path,indicator='speciesRichness',ags=c('ALL','BIOMASS'),groups='nafo',qadjusted=F)
	#indicatorSpaceTime(path=path,indicator='ratioIndicator',ags=c(10,'BIOMASS',T,11),groups='nafo',qadjusted=F)
	#indicatorSpaceTime(path=path,indicator='largeFishIndicator',ags=c('ALL','BIOMASS',T,10),groups='esswss',qadjusted=F)
	a <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(60,'BIOMASS',user.defined=F),groups='esswss',qadjusted=T,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=F,yrs=1970:2015)
	#indicatorSpaceTime(path=path,indicator='MeanTLLandings',ags=c('ALL','BIOMASS',F,11),groups='esswss',qadjusted=F)
	#indicatorSpaceTime(path=path,indicator='shannon',ags=c('ALL','ABUNDANCE',F,11),groups='esswss',qadjusted=F)
	#indicatorSpaceTime(path=path,indicator='LandByGroup',ags=c('ALL','ABUNDANCE',F,11),groups='esswss',qadjusted=F)
	#q
	a <- indicatorSpaceTime(path=path,indicator='meanTrophicLevelCommunityLength',ags=c('ALL','BIOMASS',user.defined=F),groups='esswss',qadjusted=T,biomass.data.from.qcorr=T,qadjPostStrat=F,saveIndicatorData=T)
	#not q
	indicatorSpaceTime(path=path,indicator='meanTrophicLevelCommunityLength',ags=c('ALL','BIOMASS',user.defined=F),groups='esswss',qadjusted=F,biomass.data.from.qcorr=T,qadjPostStrat=F,saveIndicatorData=F)
	
# -------- #Section 4. Combine all idnicators extracted in Section 2  #  ####
  # If indicator data need to be combined
  # This will combine all indicators that were saved 
  # individually as csv in output/Estimated Indicators (folder name: MONTH-DAY)
  # and will save them in output/Combined Indicators 
  # Important: this will NOT work if there is more than one folder in output/Estimated Indicators (remove previous folder with old extractions)
  # Plots will NOT work if there are files other than the ones exracted in output/Combined Indicators 

 source(paste(path,'/R/amc helpers.R',sep=""))
 loadfun(path)

combine.indicator=T
	if(combine.indicator) {
		inds <- c('strat','nafo','esswss','shelf')
		for(i in 1:length(inds)) {
			combineIndicators(path,ind=inds[i],stratifiedQ=T)
		 	combineIndicators(path,ind=inds[i],stratifiedQ=F)
			}	
		}
		
j = 'esswss'
i = 1
plotter = T
	if(plotter) {
		indis <- read.csv(file.path(path,"extra info","variableNameRecode.csv"),header=F)[,1]
		areas <- c('strat','nafo','esswss','shelf')
		f <- file.path(path,"output","plots","stratq-setq-noq")
		dir.create(f,recursive=T,showWarnings=F)
		m=0
		for(j in 1:length(areas)) {
			pdf(paste(f,"/",areas[j],".pdf",sep=""))
			for(i in 1:length(indis)) {
			m=m+1
			print(m)
						makePlots(path=path,indicator=indis[i],groups=areas[j])
			}
			dev.off()
		}
	}		

dev.off()

#species list

#path <- file.path('F:/Indicators')
#source(paste(path,'/src/amc helpers.R',sep=""))
#loadfun(path)
#indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(23,'BIOMASS',user.defined=T),groups='unit2redfish',qadjusted=F)
#indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(23,'BIOMASS',user.defined=T),groups='unit3redfish',qadjusted=F)
#indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(16,'BIOMASS',user.defined=T),groups='pollockeast',qadjusted=F)
#indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(16,'BIOMASS',user.defined=T),groups='pollockwest',qadjusted=F)
#indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(14,'BIOMASS',user.defined=T),groups='silverhake',qadjusted=F)

# -------- #Section 5. Extract IndiSeas indicators  #  ####

#November 12, 2013 03:58:08 PM 
#for the Indiseas Indicators

deb=F
spp <- c(2211,2526,10,11,12,13,14,15,16,19,23,30,31,40,41,42,43,44,50,60,64,70,112,114,122,123,143,150,156,160,200,201,202,203,204,220,241,300,301,304,306,320,340,350,400,410,501,502,610,619,620,622,623,630,640,641,642,647,712,880,4511)
if(deb) {
for(i in 1:length(spp)) {
indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(spp[i],'BIOMASS',user.defined=T),groups='indiseas',qadjusted=F,biomass.data.from.qcorr=T ,saveIndicatorData=T,qadjPostStrat=F,yrs=1970:2013)
}
}

		indicator.redo1=F
		start.i=1
		if(indicator.redo1) {
		print('Here we go.....')
			ex <- read.csv(file.path(path,"extra info","extraction infoOct22.csv"))
			ex <- ex[ex$rerun==1,]
			areas <- c('indiseas')
			for(i in start.i:nrow(ex)) {
					for(j in 1:length(areas)) {
						if(ex[i,5]==1)	indicatorSpaceTime(path=path,indicator=ex[i,1],ags=c(ex[i,2],ex[i,3],user.defined=F),groups=areas[j],qadjusted=F,biomass.data.from.qcorr=T,qadjPostStrat=F,yrs=1970:2013)
						}	
					}
				}	
			
# -------- #Section 6. Extract biomass (q) per species at the wssess scale#  ####
		# Indidir <- "C:/RProjects/ExtractIndicators/output/Estimated Indicators/12-06/Qadj/stratifiedLevel/"
		# Indifiles <- list.files(path = paste(Indidir, sep=""), 
		#                        pattern = "\\.csv$", full.names = F)
		
		#xp =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,812,813,814,815,816,817,818,819,820,821,822,823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,838,839,840,841,842,843,844,845,846,847,848,849,850,851,852,853,854,855,856,857,858,859,860,861,862,863,864,865,866,867,868,869,870,871,872,873,874,875,876,877,878,879,880,881,882,883,884,885,886,887,888,889,890,891,892,893,894,895,896,897,898,899,900,901,902,903,904,905,906,907,908,909,910,911,912,913,914,915,916,917,918,919,920,921,922,923,924,925,926,927,928,929,930,931,932,933,934,935,936,937,938,939,940,941,942,943,944,945,946,947,948,949,950,951,952,953,954,955,956,957,958,959,960,961,962,963,964,965,966,967,968,969,970,971,972,973,974,975,976,977,978,979,980,981,982,983,984,985,986,987,988,989,990,991,992,993,994,995,996,997,998)
		xp = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,812,813,814,815,816,817,818,819,820,821,822,823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,838,839,840,841,842,843,844,850,851,852,853,854,855,856,860,861,862,863,865,866,867,868,869,870,871,872,873,874,875,876,877,878,879,880,890,900,910,920,921,922,930,931,932,933,940,950,960,965,1099,1100,1200,1210,1211,1212,1215,1219,1220,1221,1222,1223,1224,1225,1226,1227,1228,1229,1230,1231,1232,1233,1234,1235,1236,1237,1238,1239,1240,1241,1242,1243,1244,1245,1246,1247,1248,1249,1250,1251,1252,1253,1260,1295,1300,1305,1309,1310,1311,1312,1313,1314,1315,1316,1400,1410,1420,1430,1440,1500,1510,1511,1520,1530,1600,1700,1701,1725,1750,1800,1810,1815,1820,1821,1822,1823,1824,1825,1826,1827,1828,1830,1831,1840,1845,1900,1910,1920,1930,1931,1932,2000,2001,2010,2100,2200,2210,2211,2212,2213,2214,2215,2219,2220,2221,2240,2245,2300,2309,2310,2311,2312,2313,
		       2314,2315,2316,2317,2318,2319,2320,2321,2330,2331,2332,2333,2350,2351,2352,2400,2401,2410,2411,2412,2413,2414,2415,2416,2417,2420,2421,2450,2499,2500,2508,2509,2510,2511,2512,2513,2514,2515,2519,2520,2521,2522,2523,2524,2525,2526,2527,2528,2529,2531,2532,2533,2535,2536,2537,2540,2541,2550,2551,2554,2555,2556,2558,2559,2560,2561,2562,2563,2564,2565,2566,2567,2568,2600,2605,2606,2610,2611,2612,2613,2620,2621,2622,2623,2624,2650,2699,2700,2710,2711,2712,2713,2721,2722,2731,2732,2733,2734,2735,2800,2805,2806,2809,2810,2811,2812,2813,2814,2815,2818,2819,2820,2821,2822,2823,2824,2825,2827,2828,2829,2830,2831,2832,2833,2834,2835,2836,2837,2838,2839,2840,2841,2842,2843,2844,2845,2846,2847,2848,2849,2850,2851,2852,2853,2854,2855,2856,2857,2858,2859,2860,2861,2862,2865,2866,2867,2870,2871,2872,2873,2874,2875,2876,2880,2881,2882,2883,2884,2885,2891,2892,2893,2894,2895,2896,2897,2899,2900,2901,2902,2903,2904,2905,2906,2907,2908,2909,2910,2911,2912,2913,2914,2915,2916,2917,2918,2919,2920,2921,2922,2923,2924,2925,2926,2927,2928,2929,2930,2931,2932,2933,2934,2935,2936,2937,2938,2939,2940,2941,2942,2943,2945,2946,2950,2960,2961,2965,2966,2968,2970,2971,2972,2973,2974,2975,2976,2977,2978,2979,2980,2981,2982,2983,2984,2985,2986,2987,2988,2989,2990,2991,2992,2993,2994,2995,2996,2997,2998,2999,3000,3030,3050,3051,3085,3100,3101,3102,3103,3104,3105,3106,3107,3108,3109,3110,3111,3112,3113,3114,3115,3116,3117,3118,3119,3120,3121,3122,3123,3124,3125,3126,3127,3128,3129,3130,3131,3132,3133,3134,3135,3136,3137,3138,3139,3140,3141,3142,3143,3144,3145,3146,3147,3148,3149,3150,3151,3152,3153,3154,3155,3156,3157,3158,3159,3160,3161,3162,3163,3164,3165,3166,3167,3168,3169,3170,3171,3172,3173,3174,3175,3176,3177,3179,3180,3181,3182,3183,3185,3187,3190,3191,3194,3195,3196,3200,3210,3211,3212,3221,3222,3230,3240,3270,3280,3300,3311,3312,3313,3314,3320,3330,3340,3400,3450,3451,3460,3500,3501,3502,3550,3551,3600,3601,3700,4000,4005,4110,4200,4201,4202,4209,4210,4211,4212,4216,4221,4222,4223,4224,4225,4227,4228,4229,4230,4231,4232,4233,4235,4240,4250,4251,4252,4253,4254,4255,4256,4257,4258,4259,4260,4300,4301,4302,4303,4304,4305,4306,4307,4308,4309,4310,4311,4312,4313,4314,4315,4316,4317,4318,4319,4320,4321,4322,4323,4324,4325,4326,4327,4328,4329,4330,4331,4332,4333,4334,4335,4336,4337,4338,4339,4340,4341,4342,4343,4344,4345,4346,4347,4350,4351,4352,4353,4354,4355,4356,4360,4361,4370,4380,4381,4400,4410,4420,4430,4431,4500,4501,4502,4503,4504,4510,4511,4512,4513,4514,4515,4516,4517,4518,4519,4520,4521,4522,4523,4524,4525,4526,4527,4528,4529,4530,4531,4532,4533,4534,4535,4536,4537,4538,4539,4540,4541,4542,4543,4544,4545,4546,4547,4548,4549,4550,4551,4552,4553,4554,4556,4557,4558,4559,4560,4561,4562,4563,4564,4565,4566,4567,4568,4569,4570,4571,4572,4573,4574,4575,4576,4577,4578,4579,4580,4581,4582,4583,4584,4585,4586,4587,4588,4589,4590,4591,4592,4593,4594,4595,4596,4600,4611,4621,4622,4700,4711,4712,4713,4714,4720,4999,5000,5011,5012,5100,5101,5102,5200,5201,5500,6000,6100,6110,6111,6113,6115,6117,6119,6121,6123,6125,6127,6128,6129,6130,6131,6132,6133,6134,6135,6200,6211,6212,6213,6215,6217,6300,6309,6310,6397,6400,6411,6412,6413,6420,6421,6500,6511,6600,6611,6700,6705,6710,6711,6712,6713,6714,6715,6716,6717,6718,6719,6720,6801,6900,7000,7100,7110,7111,7113,7116,7500,7600,7700,8000,8100,8111,8113,8200,8208,8210,8300,8311,8313,8315,8316,8317,8318,8319,8320,8321,8322,8323,8324,8400,8405,8411,8421,8500,8511,8520,8600,8610,8615,8618,8621,8622,8700)
		out = c()
		for(i in 1:length(xp)) {
		  #BH <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(xp[i],'BIOMASS',user.defined=T),groups='esswss',qadjusted=F,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=F,yrs=1970:2015)
		  #BH <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(xp[i],'BIOMASS',user.defined=T),groups='nafo',qadjusted=F,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=F,yrs=1970:2015)
		  #BH <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(xp[i],'BIOMASS',user.defined=T),groups='esswss',qadjusted=T,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=T,yrs=1970:2015)
		  BH <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(xp[i],'BIOMASS',user.defined=T),groups='nafo',qadjusted=T,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=T,yrs=1970:2015)
		  BH$Species <- xp[i]
		  out = rbind(out,BH)
		  }
		
		# To read/edit all of this go to: Read&DecoupleIndicators.R
		#or one at a time: 
		  
		 BHerring <- indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(60,'BIOMASS',user.defined=T),groups='strat',qadjusted=T,biomass.data.from.qcorr=T,saveIndicatorData=F,qadjPostStrat=T,yrs=1970:2015)
		             indicatorSpaceTime(path=path,indicator='resourcePotential',ags=c(4355,'BIOMASS',user.defined=T),groups='esswss',qadjusted=T,biomass.data.from.qcorr=T,saveIndicatorData=T,qadjPostStrat=T,yrs=1970:2015)
		 write.csv(BHerring, "C:/RProjects/UseIndicators/data/SpeciesBiomass/herring_strata.csv", row.names=FALSE)
		             # BHerring$Species <- "Herring"
		
		
		
		
		
		
		
		