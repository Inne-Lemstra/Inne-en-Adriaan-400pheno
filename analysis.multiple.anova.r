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



#Eigenschappen scheidden

Gmax 	<-	grep("Gmax", colnames(phenotypes), ignore.case = TRUE)
U8		<-	grep("U8416", colnames(phenotypes), ignore.case = TRUE)
T10		<-	grep("T10", colnames(phenotypes), ignore.case = TRUE)
T50		<-	grep("T50", colnames(phenotypes), ignore.case = TRUE)
AUC		<-	grep("AUC", colnames(phenotypes), ignore.case = TRUE)

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

##environments scheiden

AfterRipening     <- grep("AR.", colnames (phenotypes))
NaCl     <- grep("NaCL", colnames(phenotypes))
Mannitol <- grep("Mannitol", colnames(phenotypes))
Cold     <- grep("Cold", colnames(phenotypes))
Heat     <- grep("Heat", colnames(phenotypes))
ABA      <- grep("ABA", colnames(phenotypes))
Fresh    <-grep("Fresh", colnames(phenotypes))
Stratification <- grep("Stratification.", colnames (phenotypes))

#nu dit gedaan is wil ik een matrix maken van alle eigenschappen. Met daarnaast de batches en de environments.

matGmax <- matrix(NA,length(Gmax)*165,4)
vecGmax <- NULL
GmaxA <- NULL
GmaxB <- NULL
GmaxC <- NULL
GmaxABC <- NULL
GmaxD <- NULL
#voor de environments.
GmaxAR <- NULL
GmaxNa <- NULL
GmaxMa <- NULL
GmaxCo <- NULL
GmaxHe <- NULL
GmaxAB <- NULL
GmaxFr <- NULL
GmaxSt <- NULL

for(i in 1:length(Gmax)){
  vecGmax <- c(vecGmax, phenotypes[,Gmax[i]]) #hier de waarden Gmax onder elkaar zetten.

  GmaxA <- c(GmaxA,(which(Gmax[i] == batchA))) #hier kijken welke batches bij welke Gmax horen.
  GmaxB <- c(GmaxB,(which(Gmax[i] == batchB)))
  GmaxC <- c(GmaxC,(which(Gmax[i] == batchC)))
  GmaxABC <- c(GmaxABC,(which(Gmax[i] == batchABC)))
  GmaxD <- c(GmaxD,(which(Gmax[i] == batchD)))

  GmaxAR <- c(GmaxAR,(which(Gmax[i] == AfterRipening))) #hier kijken welke environments bij welke Gmax horen.
  GmaxNa <- c(GmaxNa,(which(Gmax[i] == NaCl)))
  GmaxMa <- c(GmaxMa,(which(Gmax[i] == Mannitol)))
  GmaxCo <- c(GmaxCo,(which(Gmax[i] == Cold)))
  GmaxHe <- c(GmaxHe,(which(Gmax[i] == Heat)))
  GmaxAB <- c(GmaxAB,(which(Gmax[i] == ABA)))
  GmaxFr <- c(GmaxFr,(which(Gmax[i] == Fresh)))
  GmaxSt <- c(GmaxSt,(which(Gmax[i] == Stratification)))
}
for(i in batchA[GmaxA])batchA[GmaxA][i]

#fuck it, ik maak wel functies.



matGmax[,1] <- vecGmax  #hier phenowaarden met Gmax onder elkaar zetten.


