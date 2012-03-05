##copyright Adriaan van der Graaf 2012
## een testje om een waarde D gelijk te trekken aan de waarden A,B,C en Abc eventueel.

setwd("C:/Users/Adriaan/400pheno/Inne-en-Adriaan-400pheno/")
data <- read.csv("BayShatraitsAll.csv",sep=";") #get the file BayShaTraits
#omdat data phenotypen en genotypen bij elkaar zijn, wil ik ze scheiden. Na inspectie ligt de grens op collumn 405.
phenotypes <- data[,1:404]					#alle data fenotypen
genotypes  <- data[,405:ncol(data)]			#alle data genotypen

#nu de individuele .A, .B, .C isoleren



phenoA <- phenotypes[,seq(1,270,6)]
phenoB <- phenotypes[,seq(2,270,6)]
phenoC <- phenotypes[,seq(3,270,6)]
#nu kijken of ABC dezelfde waarden geeft als het gemiddelde van phenoA, phenoB en phenoC
phenoABC <- phenotypes[,seq(3,270,6)]

ABCphenoavg <- matrix(0,nrow(phenoA),ncol(phenoA)) #definieren wat het gemiddelde van A, B en C zal worden
ABCdeltaphenomat <- matrix(0,nrow)phenoA),ncol(phenoA)) # definieren wat het verschil zal zijn tussen waarden van A

for(i in 1:ncol(phenoA)){
  phenomat <- cbind(phenoA[,i],phenoB[,i],phenoC[,i]) #zet kolom i van phenoA, phenoB en phenoC in een matrix phenomat
  ABCphenoavg[,i] <- rowMeans(phenomat) #reken het gemiddelde per rij uit van phenoA, phenoB en phenoC
}

ABCphenoavg[,1]
phenoABC[,1]