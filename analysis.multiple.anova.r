##copyright Adriaan van der Graaf
#het bepalen van verschillende p waarden voor de environment, de en voor het genotype.

#waarbij dit is gegeven: lm(y2~as.f(plot)+as.f(env)+as.f(genotype)

#data laden
data <- read.csv("BayShatraitsAll.csv",sep=";")
phenotypes <- data[,1:404]
genotypes <- data[,405:ncol(data)]
phenotypes <- phenotypes[-c(1,2),]
genotypes <- genotypes[-c(1,2),]
genotypes <- apply(genotypes,2,as.character)
##einde data laden


#hier een vector maken met de laatste 5 en 7 characters van de colnames van phenotypes.
last5char <- NULL
last7char <- NULL
for(i in 1:ncol(phenotypes)){
  last5char <- c(last5char,substr(colnames(phenotypes)[i],(nchar(colnames(phenotypes)[i])-4),nchar(colnames(phenotypes)[i])))
  #voor de .ABC omgeving een laatste 7 char
  last7char <- c(last7char,substr(colnames(phenotypes)[i],(nchar(colnames(phenotypes)[i])-6),nchar(colnames(phenotypes)[i])))
}



##phenotypes op batch scheiden. (A,B,C, ABC, D)

batchA   <- grep(".A", last5char,fixed = T,ignore.case=F)
batchB   <- grep(".B", last5char,fixed = T,ignore.case=F)
batchC   <- grep(".C", last5char,fixed = T,ignore.case=F)
batchABC <- grep(".ABC", last7char,fixed = T,ignore.case=F)
batchD   <- grep(".D", last5char,fixed = T,ignore.case=F)

#omdat batchA nog een paar dubbelen heeft .AB en .ABC, die eruithalen
#het lukte niet om het te automatiseren, dus 'gewoon' manueel
batchA <- batchA[-(c(47,49,51,53,55))]

#nu een vector maken met verchillende variabelen. als de batch onbekend is, dan NA, anders: A, B, C, D of ABC
batchpheno <- seq(1:404)*0
batchpheno[1:404] <- "\"NA\""
batchpheno[batchA] <- "A"
batchpheno[batchB] <- "B"
batchpheno[batchC] <- "C"
batchpheno[batchD] <- "D"
batchpheno[batchABC] <- "ABC"


##environments scheiden


AfterRipening     <- grep("AR.", colnames (phenotypes))
NaCl     <- grep("NaCL", colnames(phenotypes))
Mannitol <- grep("Mannitol", colnames(phenotypes))
Cold     <- grep("Cold", colnames(phenotypes))
Heat     <- grep("Heat", colnames(phenotypes))
ABA      <- grep("ABA", colnames(phenotypes))
Fresh    <-grep("Fresh", colnames(phenotypes))
Stratification <- grep("Stratification.", colnames (phenotypes))

envpheno <- seq(1:404)*0
envpheno[1:404] <- "\"NA\""
envpheno[Fresh] <- "Fresh"
envpheno[AfterRipening] <- "AR"
envpheno[NaCl]  <- "NaCl"
envpheno[Mannitol] <- "Mannitol"
envpheno[Cold]  <- "Cold"
envpheno[Heat] <- "Heat"
envpheno[ABA] <- "ABA"
envpheno[Stratification] <- "Strat"


lm(phenotypes ~ as.factor(batchpheno)+as.factor(envpheno)+as.factor(genotypes[,1]))
