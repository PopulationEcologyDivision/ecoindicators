#On March 2017 Adam found out that lobseter were not extracted properly from MARFIS
#landings.R therefore needs to update the lobster data and this code was used for that - make sure files gets updated in landings.rData
library(plyr)
load("C:/RProjects/ExtractIndicators/data/landings/landings.RData")
head(land)

lobster <- read.csv("C:/RProjects/ExtractIndicators/extra info/LobsterFeb2017.csv", header=TRUE, na.strings = "NA", sep=",", as.is=T, strip.white=T)
head(lobster)
str(lobster)
lobster$X28 <- NULL
lob = reshape(lobster,idvar='YEAR',varying=list(2:13),direction='long')
a = data.frame(time = 1:12,nn = names(lobster)[2:13])
lob = merge(lob,a,by='time',all.x=T)
head(lob)
lob$nn <- mapvalues(lob$nn, from = c("X27","X29","X30","X31A","X31B","X31","X32","X33",	"X34"	,"X35",	"X36"	,"X38"), 
                     to = c("4VN",	"4VS",	"4VS",	"4VS",	"4VS",	"4VS",	"4W",	"4X",	"4X",	"4X",	"4X",	"4X"))

names(lob)[names(lob) == 'X27'] <- 'CATCH'
names(lob)[names(lob) == 'nn'] <- 'NAFO_UNIT'
lob <- aggregate(CATCH~YEAR+NAFO_UNIT,data=lob,FUN=sum)

lob$SPECIES=622
lob$ALLNAMES='AMERICAN LOBSTER'
lob$GROUNDFISH = lob$CLUPEIDS = lob$FORAGE = lob$FLATFISH = lob$FINFISH = lob$LARGE_PELAGIC = lob$GADOIDS = lob$SKATES = NA
lob$INVERTEBRATES = 1
land <- land[!land$ALLNAMES %in% c('AMERICAN LOBSTER'), ]
land = rbind(land,lob)

fp = file.path(path,'data','landings')
save(land,file="C:/RProjects/ExtractIndicators/data/landings/landings.RData")
     

