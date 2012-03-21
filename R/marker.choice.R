##copyright Adriaan van der Graaf 2012
#kies de AA/BB waarden.

#ik wil hier een matrix ingevoerd hebben met op kolommen: trait, marker, op 1 en 2, en de AA/BB waarde op AdivBcol
marker.choice <- function(traitmat,properties,markervec, AdivBcol = 6){ #hier de traitmat met op positie 1 de trait en op 2 de marker
if(missing(traitmat)) stop("traitmat needs to be provided") #foutmeldingen
if(missing(properties)) stop ("properties need to be provided")#foutmeldingen
if(missing(markervec)) stop ("markers need to be provided")

choice <- list(1:length(markervec)) #dit wordt de lijst met markers en de voorkeur.
choice <- rep(choice,length(properties)) #lijst met voorkeuren vermenigvuldigen
names(choice) <- properties #voor het nageslacht om te kijken welke lijst nou precies waar hoort.
  for(prop in 1:length(properties)){ #per property kijken wat de vookeurs markers zijn.
  names(choice[[prop]]) <- markervec #hier de markers aan de lijsten verbinden
	traitproperty <- traitmat[grep(properties[prop], traitmat[,1]),] #hier haal je de property uit de traitnaam.
	  for(i in 1:length(markervec)){
		AorB <- sum(sign(as.numeric(as.character(traitproperty[which(markervec[i] == traitproperty[,2]),AdivBcol])))) #hier wordt bekeken welke markers er in de al gefilterde traits ziten, en gekeken of ze groter of kleiner zijn dan nul (sign) en daarna gesummd om te kijken of het totaal AA of BB hoort te zijn.
		if(AorB > 0) {choice[[prop]][i] <- "AA"} #als groter dan 0, dan is het AA
		if(AorB < 0) {choice[[prop]][i] <- "BB"} #als kleiner dan 0 dan is het BB
		if(AorB == 0) {choice[[prop]][i] <- "-"} #als 0, dan weten we het niet
	  }
  }
  return(choice) #hier de uitgifte van de lijst.
}

#voor als je alle sequences los naast elkaar wilt

Sequences<-function(traitmat,properties,markervec){
if(missing(properties))stop("There needs to be a vector with traits to be sequenced")
if(missing(traitmat)) stop("there needs to be a Matrix from were te A-B values are taken")
 #lijst met traits
Sequence<- NULL
T3<- marker.choice(traitmat,properties,markervec) #hier zitten alle sequeces
for(x in 1:length(properties)){
  Sequence<- cbind(Sequence,T3[[x]])
  }
  Sequence
}