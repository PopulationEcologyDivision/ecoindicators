  vesselCorr <- function (x) {
     # correct for different survey vessels (after, L.P Fanning 1985):
      #   to obtain Alfred Needler comparable units:
      # Lady Hammond (1982) and Alfred Needler (1983 to present) used a Western IIA Otter Trawl
      # whereas The A.T. Cameron used a Yankee 35 ft trawl between 1970 to 1981
      # for total numbers need to use appropriate wingspreads 35ft for yankee and 41ft for western (From Manning 1985; reinforced by SJ Smith 2013)
        cf = NULL
        x$CFVESSEL = 1  # initialise
      # vessel change correction factors apply to these years:
        HAM=1   #  Lady Hammond (1979 - 1981)
        ATC=2   #  A.T. Cameron (1982 - 1983)
      # species codes used by the database
        cod=10
        haddock=11
        whitehake=12
        silverhake=14
        plaicelarge=40
        plaicesmall=40
        witch=41
        yellowtail=42
        winterflounder=43
        cf$cod[HAM]         = 0.8
        cf$haddock[HAM]     = 1.0
        cf$whitehake[HAM]   = 1.0
        cf$silverhake[HAM]  = 1.0
        cf$plaicesmall[HAM] = 1   # <=28cm
        cf$plaicelarge[HAM] = 1   # > 28cm
        cf$witch[HAM]       = 0.8
        cf$yellowtail[HAM]  = 0.8
        cf$winterflounder[HAM] = 1.0
        cf$cod[ATC]         = 0.8
        cf$haddock[ATC]     = 1.2
        cf$whitehake[ATC]   = 1.0
        cf$silverhake[ATC]  = 1.0
        cf$plaicesmall[ATC] = 0.7
        cf$plaicelarge[ATC] = 1.0
        cf$witch[ATC]       = 0.8
        cf$yellowtail[ATC]  = 0.8
        cf$winterflounder[ATC] = 1.0
      attach (x)
        x$CFVESSEL[ which( (substring(MISSION,1,3)=="HAM" & SPECIES==cod)) ] = cf$cod[HAM]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="HAM" & SPECIES==witch)) ] = cf$witch[HAM]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="HAM" & SPECIES==yellowtail)) ] = cf$yellowtail[HAM]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="ATC" & SPECIES==cod)) ] = cf$cod[ATC]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="ATC" & SPECIES==haddock)) ] = cf$haddock[ATC]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="ATC" & SPECIES==plaicesmall && FLEN<=28)) ] = cf$plaicesmall[ATC]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="ATC" & SPECIES==witch)) ] = cf$witch[ATC]
        x$CFVESSEL[ which((substring(MISSION,1,3)=="ATC" & SPECIES==yellowtail)) ] = cf$yellowtail[ATC]
      detach (x)
      	x$BIOMASS = x$BIOMASS * x$CFVESSEL #similar coefficients in manning 1985      
      	x$ABUNDANCE = x$ABUNDANCE * x$CFVESSEL
      	if(any(names(x) %in% c('QBIOMASS','QABUNDANCE'))) {
      	x$QBIOMASS = x$QBIOMASS * x$CFVESSEL #similar coefficients in manning 1985      
      	x$QABUNDANCE = x$QABUNDANCE * x$CFVESSEL
      	}
      	x <- x[,-which(names(x)=='CFVESSEL')]
    return (x)
  }
