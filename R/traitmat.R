##copyright Adriaan van der Graaf 2012
#doel om data op te halen uit de analysis file.
#daarna opzetten in een matrix:
#benodigd zijn norm 

#aangenomen dat normpeaklist al aanwezig is.

#de namen van de traits en de marker in de matrix zetten



#de LOD waarden terughalen.
normpeakvalue <-vector("list",nrow(norm))
for(i in 1:nrow(norm)){ #in deze for loop wordt een lijst gemaakt met de LOD waarden erin.
  normpeakvalue [[i]] <- norm[i,(peak(norm[i,],3))]
}

LOD <- as.numeric(unlist(normpeakvalue)) #hier wordt de lijst samengevoegd in een vector
mmatrix <- cbind(mmatrix, LOD) #hier wordt de LOD waarden lijst in de mmatrix geplaatst

#nu de effect waarde uitrekenen. 
#omdat de waarden niet in een object zitten, moet ik ze er zelf even uittrekken.

createEffectMatrix <- function(genotypes,phenotypes){
  meanAA <- matrix(0,ncol(phenotypes),ncol(genotypes)) 
  meanBB <- matrix(0,ncol(phenotypes),ncol(genotypes))
    for(i in 1:ncol(genotypes)){
	  for(b in 1:ncol(phenotypes)){
	    genoA <- which(genotypes[,i]=="AA")   #in deze loop wordt het gemiddelde van een phenotype met genotype AA en BB berekend
	    genoB <- which(genotypes[,i]=="BB")   
        meanAA[b,i] <- mean(phenotypes[genoA,b],na.rm=T) #hier wordt de gemiddelde in een matrix geplaatst met de hoeveelheid phenotypen als rij, en de hoeveelheid genotypen als kolom
	    meanBB[b,i] <- mean(phenotypes[genoB,b],na.rm=T)
	  }
    }
 AAdivBBmat <- matrix(0,ncol(phenotypes),ncol(genotypes))   
 colnames(AAdivBBmat) <- colnames(genotypes)
 rownames(AAdivBBmat) <- colnames(phenotypes)
   for(i in 1:nrow(AAdivBBmat)){                     #in deze loop worden de gemiddelden 	door elkaar gedeeld
	 for(n in 1:ncol(AAdivBBmat)){
	   AAdivBBmat[i,n] <- ((meanAA[i,n])/(meanBB[i,n])) #hier vindt de deel-actie plaats voor alle waarden in mmatrix
     }
   }
}

createTraitMatrix <- function(genotypes,phenotypes){
  effectm <- createEffectMatrix(genotypes,phenotypes)
}
#nu de effect waarde uitrekenen. 
#omdat de waarden niet in een object zitten, moet ik ze er zelf even uittrekken.
meanAA <- matrix(0,ncol(phenotypes),ncol(genotypes)) 
meanBB <- matrix(0,ncol(phenotypes),ncol(genotypes))
for(i in 1:ncol(genotypes)){
	for(b in 1:ncol(phenotypes)){
	    genoA <- which(genotypes[,i]=="AA")   #in deze loop wordt het gemiddelde van een phenotype met genotype AA en BB berekend
	    genoB <- which(genotypes[,i]=="BB")   
		meanAA[b,i] <- mean(phenotypes[genoA,b],na.rm=T) #hier wordt de gemiddelde in een matrix geplaatst met de hoeveelheid phenotypen als rij, en de hoeveelheid genotypen als kolom
		meanBB[b,i] <- mean(phenotypes[genoB,b],na.rm=T)
	}
}

mmatrix <- cbind(mmatrix, AAdivBB) #hier wordt de mmatrix gevuld met effectwaarden.
colnames(mmatrix) <- c("Trait", "Marker", "LOD", "effect AA/BB")

}