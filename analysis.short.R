##copyright Adriaan van der Graaf/Inne Lemstra 2012

#je hoort nu in de folder onder 400pheno te zitten

source("400pheno/R/find.peaks.R")
source("400pheno/R/effect.matrix.R")
source("400pheno/R/t.test.R")
source("400pheno/R/trait.marker.list.R")
source("400pheno/R/anova.mat.R")
source("400pheno/R/T en A matrix.r")
source("400pheno/R/chr_finder.r")
#voor de multiple anova
source("400pheno/R/Grep.term.col.R") #functies laden
source("400pheno/R/M.matcher.R")
#voor het mergen van de properties.
source("400pheno/R/properties.merge.R")
#voor het maken van de plotjes van de multiple Anova
source("400pheno/R/potje.plotje.r")

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

#hier wordt de multiple anova berekend.
##########################################################################################333

properties <- c("Gmax","U8","T10","T50","AUC")#de eigenschappen scheiden.

listproperties <- grep.term.col(phenotypes,properties,0) #een lijst met matrices maken met de waarden van de properties.

batch.B.C.D <- c(".B",".C",".D")
batch.A. <- ".A."
batch.A  <- ".A"
batch.ABC <- ".ABC"

listbatchBCD <- grep.term.col(phenotypes,batch.B.C.D,5) #hier de batches BCD krijgen.
listbatchABC <- grep.term.col(phenotypes,batch.ABC,7) #hier voor ABC
#.A is een speciaal verhaal. komt erop neer dat ik twee keer check, eerst op .A. met een cutoff van 5 en daarna op .A met een cutoff van 2. 
listbatchA <- cbind(grep.term.col(phenotypes,batch.A.,5)[[1]],grep.term.col(phenotypes,batch.A,2)[[1]]) 

#enviornments
environments <- c("AR.","NaCl","Mannitol","Cold","Heat","ABA","Fresh","Stratification")

listenvironments <- grep.term.col(phenotypes,environments,0)

 batches <- vector("list",length(properties))
for(i in 1:length(listproperties)){
  batches[[i]] <- listproperties[[i]]
  batches[[i]] <- M.matcher(batches[[i]],listbatchA,"A")
  batches[[i]] <- M.matcher(batches[[i]],listbatchABC[[1]],"ABC")
  for (n in 1:length(listbatchBCD)){batches[[i]] <- M.matcher(batches[[i]],listbatchBCD[[n]],names(listbatchBCD)[n])}
}
names(batches) <- c("Gmax","U8","T10","T50","AUC")

environmat <- vector("list",length(properties))
for(i in 1:length(listproperties)){
  environmat[[i]] <- listproperties[[i]]
  for (n in 1:length(environments)){environmat[[i]] <- M.matcher(environmat[[i]],listenvironments[[n]],names(listenvironments)[n])}
}

listpropvec <- vector("list",length(listproperties)) #de hele matrix achter elkaar zetten per kolom.
for (i in 1:length(listproperties)){
  for (n in 1:ncol(listproperties[[i]])) {listpropvec[[i]] <- c(listpropvec[[i]],listproperties[[i]] [,n])}
}

listbatchvec <- vector("list",length(batches)) 
for (i in 1:length(batches)){ #de hele matrix per kolom achter elkaar in een hele lange vector zetten. X de lijsten
    for (n in 1:ncol(batches[[i]])) {listbatchvec[[i]] <- c(listbatchvec[[i]],batches[[i]] [,n])}
  listbatchvec[[i]] [which(!unlist(lapply(unlist(lapply(listbatchvec[[i]],as.numeric)),is.na)))] <- NA  # hier de waarden die niet omgezet waren in een batch, omzetten naar NA
}
 
listenvironvec <- vector("list",length(environmat)) 
for (i in 1:length(environmat)){ #de hele matrix per kolom achter elkaar in een hele lange vector zetten. X de lijsten.
    for (n in 1:ncol(environmat[[i]])) {listenvironvec[[i]] <- c(listenvironvec[[i]],environmat[[i]] [,n])}
  listenvironvec[[i]] [which(!unlist(lapply(unlist(lapply(listenvironvec[[i]],as.numeric)),is.na)))] <- NA #hier alle niet omgezette waarden in NA veranderen.
}


#anova gedeelte
Pfac <- vector("list",length(listpropvec))
Pfac.uncut <- vector("list",length(listpropvec))
Efac<-vector("list",length(listpropvec))
for (i in 1:length(listpropvec)){  
  for (n in 1:ncol(genotypes)){  
    Pfac[[i]] <- rbind(Pfac[[i]],anova(lm(listpropvec[[i]]~as.factor(listbatchvec[[i]])+as.factor(listenvironvec[[i]])+as.factor(rep(genotypes[,n],74))))$Pr)
    Efac[[i]] <- rbind(Efac[[i]],unlist(lm(listpropvec[[i]]~as.factor(listbatchvec[[i]])+as.factor(listenvironvec[[i]])+as.factor(rep(genotypes[,n],74))))[14])

  }
  Pfac[[i]] <- -log10(Pfac[[i]])
  rownames(Pfac[[i]]) <- colnames(genotypes)
  colnames(Pfac[[i]]) <- c("Batch","Environment","Genotype","Residuals")
  rownames(Efac[[i]]) <- colnames(genotypes)
  Pfac.uncut[[i]] <- Pfac[[i]][,3]
  Pfac[[i]] <-Pfac[[i]][-which(Pfac[[i]][,3] < 3),3] #hier alle waarden die kleiner zijn dan 3 eruit halen

}
names(Pfac) <- properties
names(Pfac.uncut) <- properties
names(Efac) <- properties


##hier de multiple Anova met coefficients.
MultiAnovamarker <- NULL
MultiAnovatrait <- NULL
MultiAnovaLOD <- NULL
MultiAnovaCoe <- NULL

for (i in 1:length(Pfac)){
  MultiAnovatrait <- c(MultiAnovatrait, rep(names(Pfac)[i],length(Pfac[[i]])))
    for (n in 1:length(Pfac[[i]])){
      MultiAnovamarker <- unlist(c(MultiAnovamarker, names(Pfac[[i]])[n]))
	  MultiAnovaLOD <- as.vector(unlist(c(MultiAnovaLOD, Pfac[[i]][n])))
	  MultiAnovaCoe <- unlist(c(MultiAnovaCoe, Efac[[i]][n]))
    }
}  

MultiAnova <- cbind(MultiAnovatrait,MultiAnovamarker, MultiAnovaLOD, MultiAnovaCoe)
##############
#hier de properties van de multiple anova mergen.
######

TAAmerge <- properties.merge(CombiMatrix,Pfac)



# hier de missende AA/BB waarden invullen
isNA.div <- which(is.na(TAAmerge[,5]) == T) #welke zijn NA.
tempvec12 <- NULL

#omdat de AA-BB waarden hier nog niet bij staan, en de AA/BB waarden. neem ik degemiddelden van deze waarden. 
for (i in isNA.div){ #voor de AA/BB waarden.
	ADIVBcols <-grep(paste(TAAmerge[i,1]),names(effect.mat.div[,paste(TAAmerge[i,2])])) #zoek naar de missende waarden 
	tempvec12 <-c(tempvec12,mean(effect.mat.div[ADIVBcols,paste(TAAmerge[i,2])]))
}
TAAmerge[,5] <- as.vector(TAAmerge[,5])
TAAmerge[isNA.div,5] <- tempvec12 #hier de omzetting van de AA/BB

# hier de missende AA-BB waarden invullen
#hier de coefficients
isNA.coe <- which(is.na(TAAmerge[,6]) == T) #welke zijn NA.
tempvec11 <- NULL

#omdat de AA-BB waarden hier nog niet bij staan, en de AA/BB waarden. neem ik de coefficienten. uit de Efac.
for (i in isNA.coe){
  tempvec11 <- c(tempvec11, Efac[[paste(TAAmerge[i,1])]][TAAmerge[i,2]]) #lees hier de markers en de traits uit in de Efac.
}
TAAmerge[,6] <- as.vector(TAAmerge[,6])  #zet het in de TAAmerge.
TAAmerge[isNA.coe, 6] <- as.vector(unlist(tempvec11))

#plotten van hele t.test en anova (zonder cuttoff)
T.test_raw<-as.numeric(apply(tmat,2,mean))
Anova_raw<-as.numeric(apply(anovamat,2,mean))
AminB_waarden<-sign(as.numeric(apply(effect.mat.min,2,mean)))
chromos<-data[1,405:ncol(data)]
Morgan<-data[2,405:ncol(data)]
setwd("C:/github/400pheno/images")
for (i in 1:length(Pfac.uncut)){
  png(filename=paste("Trait ",names(Pfac.uncut)[i],".png"),bg="white",height=1000, width=1000)
  plotInne(Morgan, chromos, First_line=T.test_raw, Second_line=Anova_raw,yass2=AminB_waarden,cuttoff=3,Title=paste("trait ",names(Pfac.uncut)[i]),Grote_assen=1) #de mooie functie van inne gebruiken en de rest is opmaak.
  legend("topright", c("chromosome 1","chromosome 2","chromosome 3","chromosome 4","chromosome 5","A-B"),lty=rep(1,5),lwd=rep(3,5), col=c(1:5,"purple"))
  dev.off()
}

#plotten van Pfac.
chromos<-data[1,405:ncol(data)]
Morgan<-data[2,405:ncol(data)]
setwd("C:/github/400pheno/images")
for (i in 1:length(Pfac.uncut)){
  png(filename=paste("Trait ",names(Pfac.uncut)[i],".png"),bg="white",height=1000, width=1000)
  plotInne(Morgan, chromos, as.numeric(Pfac.uncut[[i]]), cuttoff=3,Title=paste("trait ",names(Pfac.uncut)[i]),Grote_assen=1) #de mooie functie van inne gebruiken en de rest is opmaak.
  legend("topright", c("chromosome 1","chromosome 2","chromosome 3","chromosome 4","chromosome 5"),lty=rep(1,5),lwd=rep(3,5), col=1:5)
  dev.off()
}
