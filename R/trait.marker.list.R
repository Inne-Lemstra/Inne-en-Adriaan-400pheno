## copyright Adriaan van der Graaf
#het maken van een matrix met de traits die opgekomen waren uit een find peaks functie.

#de matrix beginnen met de traits en de markers op de eerste kolommen.
trait.marker.list <- function(a){ #waarbij a de lijst is die aangeleverd wordt met de traits als naam en de markers als inhoud van de lijst.
 if(!is.list(a)) stop("object a is not a list")
  mmatrix <- NULL
  for(i in 1:length(a)){ 
    for(e in a[[i]]){
      mmatrix <- rbind(mmatrix,c(names((a)[i]),e))
    }
  }
  mmatrix
}