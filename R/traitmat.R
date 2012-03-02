##copyright Adriaan van der Graaf 2012
#doel om data op te halen uit de analysis file.
#daarna opzetten in een matrix:


#aangenomen dat normpeaklist al aanwezig is.
traitmat <- matrix(0, sum(as.numeric(unlist(lapply(normpeaklist,length)))),4)


#de namen van de traits en de marker in de matrix zetten
mmatrix <- NULL
for(i in 1:length(normpeaklist)){
  for(e in normpeaklist[[i]]){
    mmatrix <- rbind(mmatrix,c(names((normpeaklist)[i]),e))
  }
}

#de LOD waarden terughalen.
normpeakvalue <-vector("list",nrow(norm))
for(i in 1:nrow(norm)){
  normpeakvalue [[i]] <- norm[i,(peak(norm[i,],3))]
}

LOD <- as.numeric(unlist(normpeakvalue))
mmatrix <- cbind(mmatrix, LOD)

#nu de effect waarde uitrekenen. 
#omdat de waarden niet in een object zitten, moet ik ze er zelf even uittrekken.

createEffectMatrix <- function(genotypes,phenotypes){

}

createTraitMatrix <- function(genotypes,phenotypes){
  effectm <- createEffectMatrix(genotypes,phenotypes)
}

meanAA <- matrix(0,ncol(phenotypes),ncol(genotypes))
meanBB <- matrix(0,ncol(phenotypes),ncol(genotypes))
for(i in 1:ncol(genotypes)){
	vectu<-NULL
	for(b in 1:ncol(phenotypes)){
	    genoA <- which(genotypes[,i]=="AA")
	    genoB <- which(genotypes[,i]=="BB")
		meanAA[b,i] <- mean(phenotypes[genoA,b],na.rm=T)
		meanBB[b,i] <- mean(phenotypes[genoB,b],na.rm=T)
	}
}
colnames(meanAA) <- colnames(genotypes)
colnames(meanBB) <- colnames(genotypes)
rownames(meanAA) <- colnames(phenotypes)
rownames(meanBB) <- colnames(phenotypes)
AAdivBB <- NULL
for(i in 1:nrow(mmatrix)){
    tempAA <- meanAA[mmatrix[i,1],mmatrix[i,2]]
	tempBB <- meanBB[mmatrix[i,1],mmatrix[i,2]]
	AAdivBB <- c(AAdivBB, (tempAA)/(tempBB))
}

mmatrix <- cbind(mmatrix, AAdivBB)
colnames(mmatrix) <- c("Trait", "Marker", "LOD", "effect AA/BB")