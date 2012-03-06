##copyrigh Adriaan van der Graaf 2012
#functie voor het maken van een effect matrix

#nu de effect waarde uitrekenen. 
#omdat de waarden niet in een object zitten, moet ik ze er zelf even uittrekken.

effect.matrix <- function(genotypes,phenotypes,genocode = c("AA","BB")){  #eerst genotypen en daarna phenotypen invullen. Dit geeft de effecten AA/BB op gemiddelden
  if(missing(genotypes)) stop("No genotypes")
  if(missing(phenotypes)) stop("No phenotypes")
  meanAA <- matrix(0,ncol(phenotypes),ncol(genotypes)) 
  meanBB <- matrix(0,ncol(phenotypes),ncol(genotypes))
    for(i in 1:ncol(genotypes)){
    for(b in 1:ncol(phenotypes)){
      genoA <- which(genotypes[,i]==genocode[1])   #in deze loop wordt het gemiddelde van een phenotype met genotype AA en BB berekend
      genoB <- which(genotypes[,i]==genocode[2])   
        meanAA[b,i] <- mean(phenotypes[genoA,b],na.rm=T) #hier wordt de gemiddelde in een matrix geplaatst met de hoeveelheid phenotypen als rij, en de hoeveelheid genotypen als kolom
      meanBB[b,i] <- mean(phenotypes[genoB,b],na.rm=T)
    }
    }
 effectAAdivBB <- matrix(0,ncol(phenotypes),ncol(genotypes))   #nu de verhouding berekenen.
 colnames(effectAAdivBB) <- colnames(genotypes)          #voor betere herkenning de namen even gelijktrekken
 rownames(effectAAdivBB) <- colnames(phenotypes)
   for(i in 1:nrow(effectAAdivBB)){                     #in deze loop worden de gemiddelden   door elkaar gedeeld
   for(n in 1:ncol(effectAAdivBB)){
     effectAAdivBB[i,n] <- ((meanAA[i,n])/(meanBB[i,n])) #hier vindt de deel-actie plaats voor alle waarden in de matrix
     }
   }
   effectAAdivBB
}

#hiervoor de genotypes en phenotypes invulen resp. En er wordt geselecteerd op AA en BB