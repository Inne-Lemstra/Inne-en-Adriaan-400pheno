##copyright Adriaan van der Graaf
#anova analyse

#verwacht wordt dat je in de folder voor Inne-en-Adriaan-400pheno zit. Daar hoort ook de data aanwezig te zijn.

source("Inne-en-Adriaan-400pheno/R/find.peaks.R")
source("Inne-en-Adriaan-400pheno/R/effect.matrix.R")
source("Inne-en-Adriaan-400pheno/R/anova.mat.R")
source("Inne-en-Adriaan-400pheno/R/trait.marker.list.R")
#data laden
data <- read.csv("BayShatraitsAll.csv",sep=";")
phenotypes <- data[,1:404]
genotypes <- data[,405:ncol(data)]
phenotypes <- phenotypes[-c(1,2),]
genotypes <- genotypes[-c(1,2),]
genotypes <- apply(genotypes,2,as.character)
##einde data laden

anovamat <- anova.mat(as.matrix(phenotypes),as.matrix(genotypes))
traitmat <- trait.marker.list(peak.mat.row(anovamat,3,phenotypes))
