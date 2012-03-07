##copyright Adriaan van der Graaf 2012

#je hoort nu in de folder onder Inne-en-Adriaan-400pheno te zitten
setwd("c:/github/")
source("400pheno/R/find.peaks.R")
source("400pheno/R/effect.matrix.R")
source("400pheno/R/t.test.R")
source("400pheno/R/trait.marker.list.R")
source("400pheno/R/anova.mat.R")
source("400pheno/R/T en A matrix.r")

#data laden
data <- read.csv("BayShatraitsAll.csv",sep=";")
phenotypes <- data[,1:404]
genotypes <- data[,405:ncol(data)]
phenotypes <- phenotypes[-c(1,2),]
genotypes <- genotypes[-c(1,2),]
genotypes <- apply(genotypes,2,as.character)
##einde data laden

MatrixT.test<-NULL
tmat <- t.test.mat(phenotypes,genotypes)  #tmat is een matrix van alle -log10 p-waarden per genotype tov phenotype
MatrixT.test <- trait.marker.list(peak.mat.row(tmat,3,phenotypes)) #MatrixT.test is een matrix van alle traits en markers waar de piek groter is dan 3. in de eerste kolom trait, 2e kolom marker.

#LODT.test tmat waarden die groter zijn dan de cutoff.
LODT.test <- NULL
for (i in 1:nrow(MatrixT.test)){
  LODT.test <- c(LODT.test, tmat[MatrixT.test[i,1],MatrixT.test[i,2]])  #wordt automatisch op volgorde gezet door de MatrixT.testrix. deze vraagt de trait en marker namen op en de waarde in de tmat
}
MatrixT.test <- cbind(MatrixT.test,LODT.test) #hier wordt de LODT.test waarde aan de trait matrix gebonden.

#AA/BB
effect.mat<- effect.matrix(genotypes,phenotypes,"divide") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec <- NULL
for (i in 1:nrow(MatrixT.test)){
   effect.vec <- c(effect.vec,effect.mat[MatrixT.test[i,1],MatrixT.test[i,2]]) #wordt automatisch op volgorde gezet door de MatrixT.testrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixT.test <- cbind(MatrixT.test,effect.vec) #hier de effect.vec vector aan de MatrixT.test gebonden.

colnames(MatrixT.test) <- c("Trait", "Marker", "LODT.test", "AA/BB") #colnames nog even gelijktrekken.

#AA-BB
effect.mat<-NULL
effect.mat<- effect.matrix(genotypes,phenotypes,"substract") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec <- NULL
for (i in 1:nrow(MatrixT.test)){
   effect.vec <- c(effect.vec,effect.mat[MatrixT.test[i,1],MatrixT.test[i,2]]) #wordt automatisch op volgorde gezet door de MatrixT.testrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixT.test <- cbind(MatrixT.test,effect.vec) #hier de effect.vec vector aan de MatrixT.test gebonden.

colnames(MatrixT.test) <- c("Trait", "Marker", "LODT.test", "AA/BB","AA-BB") #colnames nog even gelijktrekken.

#Matrix LOD Anavo

anovamat <- anova.mat(as.matrix(phenotypes),as.matrix(genotypes))
MatrixAnova <- trait.marker.list(peak.mat.row(anovamat,3,phenotypes))


#LODAnova tmat waarden die groter zijn dan de cutoff.
LODAnova <- NULL
for (i in 1:nrow(MatrixAnova)){
  LODAnova <- c(LODAnova, anovamat[MatrixAnova[i,1],MatrixAnova[i,2]])  #wordt automatisch op volgorde gezet door de MatrixAnovarix. deze vraagt de trait en marker namen op en de waarde in de tmat
}
MatrixAnova <- cbind(MatrixAnova,LODAnova) #hier wordt de LODAnova waarde aan de trait matrix gebonden.
colnames(MatrixAnova) <- c("Trait", "Marker", "LODAnova") #colnames nog even gelijktrekken.

#Complte matrix met t.test en anova
Temp<-voegsamen(MatrixT.test,MatrixAnova)
Order<-sort(colnames(Temp),decreasing=TRUE)
CombinedLod<-Temp[Order]







