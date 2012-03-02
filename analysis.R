# 400 phenotypes in A. Thaliana
# created: 20-02-12
#(c) created by Inne Lemstra



data <- read.csv("BayShatraitsAll.csv",sep=";")
phenotypes <- data[,1:404]
genotypes <- data[,405:ncol(data)]
genotypes[1:10,1:10]
genotypes[1:10,1:50]
colnames(phenotypes)
grep(".A",colnames(phenotypes))
grep(".B",colnames(phenotypes))
grep(".ABC",colnames(phenotypes))

phenotypes <- phenotypes[-c(1,2),]
genotypes <- genotypes[-c(1,2),]
genotypes <- apply(genotypes,2,as.character)
#Vragen 
# - Welke environment heeft de meeste invloed ?
# - Welke phenoypes zijn environment affected
# - Welke phenoypes zijn genetic affected
# - Try to assign a OP-value to the difference in mean (e.g. use a t.test)
# Extra anova / manova analyse v/d environment en genetics


#z-score

matr <- NULL
for(pheno in 1:404){
	gemm <-mean(phenotypes[,pheno], na.rm=TRUE)
	stand<- sd(phenotypes[,pheno], na.rm=TRUE)
	vect <- NULL
	for(a in 1:nrow(phenotypes)){
		z<- ((phenotypes[a,pheno]-gemm)/stand)
		vect <- c(vect,z)
	}
	matr <- cbind(matr,vect)
}
	image(matr, xlab= "phenotypes", ylab="genotypes", col=heat.colors(4))
	
matr_D <- image(which(matr<=0.5),which(matr>0.5 & matr<= 1.0), which(matr>1.0), col= heat.colors(3) )


apply(phenotypes[,T50c],2,function(x){which(x>0)})

#per phenotype wil ik een score die aangeeft welke genotype !! het beste correleert
#hoe parameters voor figuren maken

#vb.
op <- par(las = 2)
op<- par(cex = 0.6)
op <- par(mai=c(3,0.4,0,0))
mT10<- boxplot(as.matrix(phenotypes[,T10]),na.rm=TRUE)
#op <- par(las = 3)   		 : style van x-as labels 
#(0= altijd prara a x-as, 1= horizon , 2= altijd loodrecht a x-as, 3=verticaal) 
#op<- par(cex = 0.6)  		 : Grote X-as labels
#op <- par(mai=c(3,0.4,0,0)) : Ruimte om de GRAFIEK (onder,links,boven,rechts)

cor(phenotypes[,1],as.numeric(genotypes[,1]), use="pair")

#Matrix met correlatie waarden tusse phenotypes en markers
matri<-NULL
for(romark in 1:ncol(genotypes)){
	
	vecto<- NULL
	for(a in 1:ncol(phenotypes)){
		
		core<-cor(phenotypes[,a],as.numeric(genotypes[,romark]), use="pair")
		vecto<-c(vecto,core)
	}
	
	matri<- cbind(matri, vecto )
}

 colnames(matri) <- colnames(genotypes)
apply(matri, 2, function(x) (x>0.8))

# visualiseren matri
op <- par(las = 2)
op<- par(cex = 0.6)
op <- par(mai=c(3,0.4,0,0))

heatmap(matri)
boxplot(matri, las=2, cex=0.6, mai=c(3,0.4,0,0) )



abovethreshold <- apply(matri>0.2,2,function(x){which(x)})
nietNULL <- which(unlist(lapply(abovethreshold,function(x){!(is.na(x)&&1)})))
abovethreshold[nietNULL]

#t.test doen
t.test(phenotypes[,1],genotypes[,1], conf.level = 0.95)$p.value
matru<-NULL
for(rmark in 1:ncol(genotypes)){
	vectu<-NULL
	for(b in 1:ncol(phenotypes)){
	    genoA <- which(genotypes[,rmark]=="AA")
	    genoB <- which(genotypes[,rmark]=="BB")
		coru <- t.test(phenotypes[genoA,b],phenotypes[genoB,b], conf.level = 0.95, use="pair")$p.value
		vectu<-c(vectu,coru)
	}
	
	matru<- cbind(matru,vectu)
}

colnames(matru)<- colnames(genotypes)
norm<- -log10(matru)
plot(norm[1,],t='l')

#peak finder

#functie voor het vinden van peaks
#hier wil ik de functie diffpotential maken. 
#Deze geeft aan of er veel positieve of negatieve groei is geweest.
#dat gebeurt door te kijken of de functie achter elkaar aan het stijgen of dalen is.
# als de functie aan het dalen is, dan wordt elke waarde dat er gedaald wordt, de diff potential elke waarde -1 kleiner
#als de functie aan het stijgen is, dan wordt elke waarde gestegen de diff potential waarde +1 groter.
#als de functie gelijk blijft, dan blijft de diff potential waarde gelijk.
#als de waarde van stijg naar daal gaat, dan wordt de diffpotential waarde in 0 omgezet.

peak <- function(a,b=0){
  a <- replace(a,a<=b,0) #b is hier je cutoff waarde.
  diffpotential <- seq(1,length(a))*0   #deze waarden wil ik graag uitlezen. Als er steeds groei plaatsvindt, dan wordt de waarde steeds groter. Maar als er een keer negatieve groei plaatsvindt, dan wordt hij terug gezet op 0.
  peaks <- NULL
    for(i in 2:length(a)){
	  if(diff(a)[i-1] > 0){ #<- deze waarden geven aan of er groei of niet heeft plaatsgevonden. want als diffa[i]> 0 dan heeft er positieve groei plaatsgevonden.
	    if(diffpotential[i-1] >= 0) { diffpotential [i] <- diffpotential[i-1] +1} else{diffpotential[i] <- 0}
	  }  
	  if(diff(a)[i-1] < 0){
	    if(diffpotential[i-1] <= 0) { diffpotential [i] <- diffpotential[i-1] -1} else{diffpotential[i] <- 0}
	  }
    }
  diffpotential
  pos <- which(diffpotential > 0)  #welke zijn groter dan nul
  poszero <- (which(diffpotential[pos+1] == 0)) #welke volgende van groter dan nul zijn nul. (waar hij stopt met groeien)
  pos[poszero] #(posities van de pieken in a en diffpotential 
}

normpeaklist <- vector("list", nrow(norm))
for(i in 1:nrow(norm)){
  normpeaklist[[i]] <- colnames(norm)[peak(norm[i,],3)]
}
names(normpeaklist)<- colnames(phenotypes)
normpeaklist


