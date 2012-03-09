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

source("Inne-en-Adriaan-400pheno/R/Grep.term.col.R") #functies laden
source("Inne-en-Adriaan-400pheno/R/M.matcher.R")

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

for (i in 1:length(listpropvec)){  
  for (n in 1:ncol(genotypes)){  
    Pfac[[i]] <- rbind(Pfac[[i]],anova(lm(listpropvec[[i]]~as.factor(listbatchvec[[i]])+as.factor(listenvironvec[[i]])+as.factor(rep(genotypes[,n],74))))$Pr)
  }
  Pfac[[i]] <- -log10(Pfac[[i]])
  rownames(Pfac[[i]]) <- colnames(genotypes)
  colnames(Pfac[[i]]) <- c("Batch","Environment","Genotype","Residuals")
  Pfac[[i]] <-Pfac[[i]][-which(Pfac[[i]][,3] < 3),3] #hier alle waarden die kleiner zijn dan 3 eruit halen
}
names(Pfac) <- properties




















