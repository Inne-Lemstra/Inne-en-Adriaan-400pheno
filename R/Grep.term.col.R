##Copyright Adriaan van der Graaf
#functie voor het zoeken naar een term in de colnames

grep.term.col <- function(traits, term, cutoff=0){
  if(missing(traits)) stop("trait matrix needs to be defined")
  if(missing(term)) stop("term needs to be defined")
  if(!is.character(term)) stop("term needs to be a character")
  
  
  #functie begint werkelijk HIER
  if(cutoff == 0) {colnamesorigin <-colnames(traits)} else{ #voor het afkappen van de colnames. Alleen de laatste characters. voor de batches kan nog uitgebreid worden met tussen welke characters ik wil. Maar ik ga het niet doen.
       lastchar <- NULL
    for(i in 1:ncol(traits)){
      colnames(traits)[i] <- substr(colnames(traits)[i],(nchar(colnames(traits)[i])-(cutoff-1)),nchar(colnames(traits)[i])) #de laatste cutoff caracters geef ik.
    }
  }
  cols <- grep(term, colnames (traits))			#zoek naar de term in de colnames
  searchmat <- NULL
    for (i in 1:length(cols)){
      searchmat <- cbind(searchmat, traits[,cols[i]])  # maak een matrix met de kolommen die de term bevatten in de colnaam
    }
  colnames(searchmat)<- colnamesorigin[cols]
  return(searchmat) #geef de matrix
}
