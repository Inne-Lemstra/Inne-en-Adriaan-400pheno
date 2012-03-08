##Copyright Adriaan van der Graaf
#functie voor het zoeken naar een term in de colnames

grep.term.col <- function(traits, term, cutoff=0){
  if(missing(traits)) stop("trait matrix needs to be defined")
  if(missing(term)) stop("term needs to be defined")
  if(!is.character(term)) stop("term needs to be a character")
  for (i in 1:length(cutoff)) {if(cutoff[i] < 0) stop("Cutoff needs to be a whole number above zero.")}
  
  
  #functie begint werkelijk HIER
  colnamesorigin <-colnames(traits) #hier de colnames saven om later in de eventueel afgekapte colnames te stoppen
  
  if(cutoff[i] == 0) {} else{ #voor het afkappen van de colnames. Alleen de laatste characters. voor de batches kan nog uitgebreid worden met tussen welke characters ik wil. Maar ik ga het niet doen.
    for(i in 1:ncol(traits)){
      colnames(traits)[i] <- substr(colnames(traits)[i],(nchar(colnames(traits)[i])-(cutoff-1)),nchar(colnames(traits)[i])) #de laatste cutoff caracters geef ik.
    }
  }
  
  searchmat <- vector("list",length(term)) #hier de lijst definieren.
  for (n in 1:length(term)){ #als term langer is dan 1, dan krijgt de lijst meer opties.
    
	cols <- grep(term[n], colnames (traits))			#zoek naar de term in de colnames
    if(length(cols) == 0) {next}
      for (i in 1:length(cols)){
        searchmat[[n]] <- cbind(searchmat[[n]], traits[,cols[i]])  # maak een matrix met de kolommen die de term bevatten in de colnaam
      }
    colnames(searchmat[[n]])<- colnamesorigin[cols] #colnames terugzetten voor het geval ze afgekapt waren
  }	
  names(searchmat) <- term
  
  return(searchmat) #geef de lijst met matrices
}


batchlist <- grep.term.col(phenotypes,c(".A.",".B.",".C.",".ABC",".D."),5)