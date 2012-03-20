##copyright Adriaan van der Graaf 2012
#kies de AA/BB waarden.

#ik wil hier een matrix ingevoerd hebben met op kolommen: trait, marker, LOD waarde, en AA/BB waarde
marker.choice <- function(traitmat,properties,markervec, AdivBcol = 6){
if(missing(traitmat)) stop("traitmat needs to be provided")
if(missing(properties)) stop ("properties need to be provided")
if(missing(markervec)) stop ("markers need to be provided")

choice <- list(1:length(markervec))
choice <- rep(choice,length(properties))
names(choice) <- properties
  for(prop in 1:length(properties)){
  names(choice[[prop]]) <- markervec
	traitproperty <- traitmat[grep(properties[prop], traitmat[,1]),]
	  for(i in 1:length(markervec)){
		AorB <- sum(sign(as.numeric(as.character(traitproperty[which(markerlist[i] == traitproperty[,2]),AdivBcol]))))
		if(AorB > 0) {choice[[prop]][i] <- "AA"}
		if(AorB < 0) {choice[[prop]][i] <- "BB"}
		if(AorB == 0) {choice[[prop]][i] <- "No pref"}
	  }
  }
  return(choice)
}
