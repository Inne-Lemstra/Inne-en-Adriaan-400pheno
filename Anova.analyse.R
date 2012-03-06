##copyright Adriaan van der Graaf
#anova analyse

#verwacht wordt dat je in de folder voor Inne-en-Adriaan-400pheno zit. Daar hoort ook de data aanwezig te zijn.

source("400pheno/R/find.peaks.R")
source("400pheno/R/effect.matrix.R")
source("400pheno/R/anova.mat.R")
source("400pheno/R/trait.marker.list.R")
#data laden
data <- read.csv("BayShatraitsAll.csv",sep=";")
phenotypes <- data[,1:404]
genotypes <- data[,405:ncol(data)]
phenotypes <- phenotypes[-c(1,2),]
genotypes <- genotypes[-c(1,2),]
genotypes <- apply(genotypes,2,as.character)
##einde data laden

anovamat <- anova.mat(as.matrix(phenotypes),as.matrix(genotypes))
MatrixAnova <- trait.marker.list(peak.mat.row(anovamat,3,phenotypes))


#LODAnova tmat waarden die groter zijn dan de cutoff.
LODAnova <- NULL
for (i in 1:nrow(MatrixAnova)){
  LODAnova <- c(LODAnova, anovamat[MatrixAnova[i,1],MatrixAnova[i,2]])  #wordt automatisch op volgorde gezet door de MatrixAnovarix. deze vraagt de trait en marker namen op en de waarde in de tmat
}
MatrixAnova <- cbind(MatrixAnova,LODAnova) #hier wordt de LODAnova waarde aan de trait matrix gebonden.


effect.mat<- effect.matrix(genotypes,phenotypes) #effect matrix is de matrix met alle AA/BB waarden.
effect.vec <- NULL
for (i in 1:nrow(MatrixAnova)){
   effect.vec <- c(effect.vec,effect.mat[MatrixAnova[i,1],MatrixAnova[i,2]]) #wordt automatisch op volgorde gezet door de MatrixAnovarix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixAnova <- cbind(MatrixAnova,effect.vec) #hier de effect.vec vector aan de MatrixAnova gebonden.

colnames(MatrixAnova) <- c("Trait", "Marker", "LODAnova", "AA/BB") #colnames nog even gelijktrekken.