defineGroups <- function(dat,gp) {
	if(gp=='nafo') {
	ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),ID=c(rep('4VN',3),rep('4VS',10),rep('4W',14),rep('4X',21)))
	}
  
  if(gp=='esswssbof_mpa') {
    ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),ID=c(rep('ESS',27),rep('WSS_minusBoF_mpa',15),rep('BoF_mpa',6)))
  }
  
  if(gp=='esswssbof') {
    ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),ID=c(rep('ESS',27),rep('WSS_minusBoF',13),rep('BoF',8)))
  }
  
	if(gp=='shelf') {
	ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),ID=rep('SHELF',48))
	}
	if(gp=='esswss') {
	ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),ID=c(rep('ESS',27),rep('WSS',21)))
	}
	if(gp=='indiseas') {
	ar <- data.frame(STRAT=c(443:466,470:478,480:485,490:495),ID=c(rep('ESS',24),rep('WSS',21)))
	}

	if(gp=='strat') {
	ar <- data.frame(STRAT=c(440:466,470:478,480:485,490:495),ID=c(440:466,470:478,480:485,490:495))
	}
	if(gp=='pollockwest') {
	ar <- data.frame(STRAT=c(474,476, 480, 481, 482,483,484,485,486,487,488,489,490,491,492,493,494,495),ID=c('pollockwest'))
	}
	if(gp=='pollockeast') {
	ar <- data.frame(STRAT=c(440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,475,477,478),ID=c('pollockeast'))
	}
	if(gp=='unit2redfish') {
	ar <- data.frame(STRAT=c(440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,464),ID=c('U2redfish'))
	}
	if(gp=='unit3redfish') {
	ar <- data.frame(STRAT=c(457,458,459,460,461,462,463,465,466,467,468,469,470,471,472,473,474,475,476,477,478,480,481,482,483,484,485),ID=c('U3redfish'))
	}
	if(gp=='silverhake') {
	ar <- data.frame(STRAT=c(440:483),ID=c('silverhake'))
	}

	if(any(!is.na(dat$STRAT))) {
	dat <- merge(dat,ar,by='STRAT',all.x=T)
	} else {
	print('Data input problems, you need to identify a strata column from summer rv strata and a group (gp) which defines the group (nafo,shelf,esswss or shelf)')
	dat <- dat
	}
	return(dat)
}

