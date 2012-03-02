##copyright Adriaan van der Graaf

tmat <- t.test.mat(phenotypes,genotypes) 
traitmat <- trait.marker.list(peak.mat.row(tmat,3))

LOD <- NULL
for (i in 1:nrow(traitmat)){
  LOD <- c(LOD, tmat[traitmat[i,1],traitmat[i,2]])
}
traitmat <- cbind(traitmat,LOD)
effect.mat<- effect.matrix(genotypes,phenotypes)
effect.vec <- NULL
for (i in 1:nrow(traitmat)){
   effect.vec <- c(effect.vec,effect.mat[traitmat[i,1],traitmat[i,2]])
}
traitmat <- cbind(traitmat,effect.vec)

traitmat[1:10,]