##copyright Adriaan van der Graaf 2012

#je hoort nu in de folder onder Inne-en-Adriaan-400pheno te zitten

source("Inne-en-Adriaan-400pheno/R/find.peaks.R")
source("Inne-en-Adriaan-400pheno/R/effect.matrix.R")
source("Inne-en-Adriaan-400pheno/R/de.ttest.R")
source("Inne-en-Adriaan-400pheno/R/trait.marker.list.R")


#data laden
data <- read.csv("BayShatraitsAll.csv",sep=";")
phenotypes <- data[,1:404]
genotypes <- data[,405:ncol(data)]
phenotypes <- phenotypes[-c(1,2),]
genotypes <- genotypes[-c(1,2),]
genotypes <- apply(genotypes,2,as.character)
##einde data laden


tmat <- t.test.mat(phenotypes,genotypes,"AA","BB",0.95)  #tmat is een matrix van alle -log10 p-waarden per genotype tov phenotype
traitmat <- trait.marker.list(peak.mat.row(tmat,3,phenotypes)) #traitmat is een matrix van alle traits en markers waar de piek groter is dan 3. in de eerste kolom trait, 2e kolom marker.

#LOD tmat waarden die groter zijn dan de cutoff.
LOD <- NULL
for (i in 1:nrow(traitmat)){
  LOD <- c(LOD, tmat[traitmat[i,1],traitmat[i,2]])  #wordt automatisch op volgorde gezet door de traitmatrix. deze vraagt de trait en marker namen op en de waarde in de tmat
}
traitmat <- cbind(traitmat,LOD) #hier wordt de LOD waarde aan de trait matrix gebonden.


effect.mat<- effect.matrix(genotypes,phenotypes,"AA","BB") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec <- NULL
for (i in 1:nrow(traitmat)){
   effect.vec <- c(effect.vec,effect.mat[traitmat[i,1],traitmat[i,2]]) #wordt automatisch op volgorde gezet door de traitmatrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
traitmat <- cbind(traitmat,effect.vec) #hier de effect.vec vector aan de traitmat gebonden.

colnames(traitmat) <- c("Trait", "Marker", "LOD", "AA/BB") #colnames nog even gelijktrekken.