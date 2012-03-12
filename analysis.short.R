##copyright Adriaan van der Graaf/Inne Lemstra 2012

#je hoort nu in de folder onder Inne-en-Adriaan-400pheno te zitten
setwd("c:/github/")
source("400pheno/R/find.peaks.R")
source("400pheno/R/effect.matrix.R")
source("400pheno/R/t.test.R")
source("400pheno/R/trait.marker.list.R")
source("400pheno/R/anova.mat.R")
source("400pheno/R/T en A matrix.r")
source("400pheno/R/chr_finder.r")

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
effect.mat.div<- effect.matrix(genotypes,phenotypes,"divide") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec.div <- NULL
for (i in 1:nrow(MatrixT.test)){
   effect.vec.div <- c(effect.vec.div,effect.mat.div[MatrixT.test[i,1],MatrixT.test[i,2]]) #wordt automatisch op volgorde gezet door de MatrixT.testrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixT.test <- cbind(MatrixT.test,effect.vec.div) #hier de effect.vec vector aan de MatrixT.test gebonden.

colnames(MatrixT.test) <- c("Trait", "Marker", "LODT.test", "AA/BB") #colnames nog even gelijktrekken.

#AA-BB
effect.mat.min<-NULL
effect.mat.min<- effect.matrix(genotypes,phenotypes,"substract") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec.min <- NULL
for (i in 1:nrow(MatrixT.test)){
   effect.vec.min <- c(effect.vec.min,effect.mat.min[MatrixT.test[i,1],MatrixT.test[i,2]]) #wordt automatisch op volgorde gezet door de MatrixT.testrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixT.test <- cbind(MatrixT.test,effect.vec.min) #hier de effect.vec vector aan de MatrixT.test gebonden.

colnames(MatrixT.test) <- c("Trait", "Marker", "LODT.test", "AA/BB","AA-BB") #colnames nog even gelijktrekken.

#Matrix LOD Anavo

anovamat <- anova.mat(as.matrix(phenotypes),as.matrix(genotypes))
MatrixAnova <- trait.marker.list(peak.mat.row(anovamat,3,phenotypes))


#LODAnova tmat waarden die groter zijn dan de cutoff.
LODAnova <- NULL
for (i in 1:nrow(MatrixAnova)){
  LODAnova <- c(LODAnova, anovamat[MatrixAnova[i,1],MatrixAnova[i,2]])  #wordt automatisch op volgorde gezet door de MatrixAnovarix. deze vraagt de trait en marker namen op en de waarde in de tmat
}
MatrixAnova<-cbind(MatrixAnova,LODAnova)
#######AA/BB########
effect.mat.div<- effect.matrix(genotypes,phenotypes,"divide") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec.div <- NULL
for (i in 1:nrow(MatrixAnova)){
   effect.vec.div <- c(effect.vec.div,effect.mat.div[MatrixAnova[i,1],MatrixAnova[i,2]]) #wordt automatisch op volgorde gezet door de MatrixT.testrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixAnova <- cbind(MatrixAnova,effect.vec.div) #hier de effect.vec vector aan de MatrixT.test gebonden.

colnames(MatrixAnova) <- c("Trait", "Marker", "LODAnova", "AA/BB") #colnames nog even gelijktrekken.
########AA-BB#######
effect.mat.min<-NULL
effect.mat.min<- effect.matrix(genotypes,phenotypes,"substract") #effect matrix is de matrix met alle AA/BB waarden.
effect.vec.min <- NULL
for (i in 1:nrow(MatrixAnova)){
   effect.vec.min <- c(effect.vec.min,effect.mat.min[MatrixAnova[i,1],MatrixAnova[i,2]]) #wordt automatisch op volgorde gezet door de MatrixT.testrix. Deze vraagt de trait en marker namen op en de waarde is de AAdivBB.
}
MatrixAnova <- cbind(MatrixAnova,effect.vec.min) #hier de effect.vec vector aan de MatrixT.test gebonden.

colnames(MatrixAnova) <- c("Trait", "Marker", "LODAnova", "AA/BB","AA-BB") #colnames nog even gelijktrekken.

#MatrixAnova <- cbind(MatrixAnova,LODAnova) #hier wordt de LODAnova waarde aan de trait matrix gebonden.
#colnames(MatrixAnova) <- c("Trait", "Marker", "LODAnova") #colnames nog even gelijktrekken.

#Complete matrix met t.test en anova
Temp<-voegsamen(MatrixT.test,MatrixAnova)
Order<-sort(colnames(Temp),decreasing=TRUE)
CombiMatrix<-Temp[Order]

#lijst met chromosoom nummers corresponderend met genotypes
chromos<-data[1,405:ncol(data)]
lijsten_chr1<-per.chr(chromos,CombiMatrix[,2],1)

#plot Marker op chromosoom 1
plot(CombiMatrix[lijsten_chr1,])



