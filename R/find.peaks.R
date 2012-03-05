# Peak finding in R
# (c) 2012 By Inne en door Adriaan

#functie voor het vinden van peaks
#hier wil ik de functie diffpotential maken. 
#Deze geeft aan of er veel positieve of negatieve groei is geweest.
#dat gebeurt door te kijken of de functie achter elkaar aan het stijgen of dalen is.
# als de functie aan het dalen is, dan wordt elke waarde dat er gedaald wordt, de diff potential elke waarde -1 kleiner
#als de functie aan het stijgen is, dan wordt elke waarde gestegen de diff potential waarde +1 groter.
#als de functie gelijk blijft, dan blijft de diff potential waarde gelijk.
#als de waarde van stijg naar daal gaat, dan wordt de diffpotential waarde in 0 omgezet.

peak <- function(a,b=0){  #a is een vector, b is een cutoff waarde.
  a <- replace(a,a<=b,0)
  diffpotential <- seq(1,length(a))*0   #deze waarden wil ik graag uitlezen. Als er steeds groei plaatsvindt, dan wordt de waarde steeds groter. Maar als er een keer negatieve groei plaatsvindt, dan wordt hij terug gezet op 0.
  peaks <- NULL
    for(i in 2:length(a)){
	  if(diff(a)[i-1] >= 0){ #<- deze waarden geven aan of er groei of niet heeft plaatsgevonden. want als diffa[i]> 0 dan heeft er positieve groei plaatsgevonden.
	    if(diffpotential[i-1] >= 0) { diffpotential [i] <- diffpotential[i-1] +1} else{diffpotential[i] <- 0}
	  }  
	  if(diff(a)[i-1] < 0){
	    if(diffpotential[i-1] <= 0) { diffpotential [i] <- diffpotential[i-1] -1} else{diffpotential[i] <- 0}
	  }
    }
  diffpotential
  pos <- which(diffpotential > 0)  #welke zijn groter dan nul
  poszero <- (which(diffpotential[pos+1] == 0)) #welke volgende van groter dan nul zijn nul. (waar hij stopt met groeien)
  pos[poszero] #(posities van de pieken in a en diffpotential 
}

#vb. peak(c(seq(1,10),seq(-1,-10),0) nu zoek je in een vector 1,2,3,4,5,6,7,8,9,10,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10 de positieve pieken die boven nul uitkomen. wel een vector invullen.

# deze functie is om per rij een vector te maken met alle pieken die boven de cutoff uitkomen.
peak.mat.row <- function(a,b=0,trait) { #a is een matrix, b is een cutoff waarde en trait is de file die je wilt gebruiken voor je phenotypen
  peak.mat <- vector("list", nrow(a))
  for(i in 1:nrow(a)){
    peak.mat[[i]] <- colnames(a)[peak(a[i,],b)]    #hier wordt de peaks functie gebruikt per trait. Om zo de markers aan te geven die significant zijn.
  }
  names(peak.mat) <- colnames(trait[1:length(peak.mat)])
  peak.mat
}



