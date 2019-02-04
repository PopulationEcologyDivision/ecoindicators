#TEST
library(data.table)
load("C:/RProjects/ExtractIndicators/data/landings/landings_OLD.RData") 

load("C:/RProjects/ExtractIndicators/data/landings/landings.RData")
load("C:/RProjects/ExtractIndicators/data/landings/data/landings/landings.RData")

DT <- as.data.table(land)

DT[FORAGE == "1"]
DT[YEAR == "2015"]


