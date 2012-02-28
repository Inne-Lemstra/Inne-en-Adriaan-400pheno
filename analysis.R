# 400 phenotypes in A. Thaliana
# created: 20-02-12
#(c) created by Inne Lemstra

setwd("X:/computational/400")

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

#environment
grep("NaCl", colnames (phenotypes))
grep("Mannitol", colnames(phenotypes))
grep("Cold", colnames(phenotypes))
grep("Heat", colnames(phenotypes))
grep("ABA", colnames(phenotypes))


#eigenschappen
Gmax <-grep("Gmax", colnames(phenotypes), ignore.case = TRUE)
U8<-grep("U8416", colnames(phenotypes), ignore.case = TRUE)
T10<-grep("T10", colnames(phenotypes), ignore.case = TRUE)
T50<-grep("T50", colnames(phenotypes), ignore.case = TRUE)
AUC<-grep("AUC", colnames(phenotypes), ignore.case = TRUE)

#eigenschappen zonder stratification
Gmaxc <-Gmax[-which(grep("Gmax", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
U8c<-U8[-which(grep("U8416", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
T10c<-T10[-which(grep("T10", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
T50c<-T50[-which(grep("T50", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
AUCc<-AUC[-which(grep("AUC", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]

#boxplot
mGmax<- boxplot(as.matrix(phenotypes[,Gmax]),na.rm=TRUE)
mU8<- boxplot(as.matrix(phenotypes[,U8]),na.rm=TRUE)
mT10<- boxplot(as.matrix(phenotypes[,T10]),na.rm=TRUE)
mT50<- boxplot(as.matrix(phenotypes[,T50]),na.rm=TRUE)
mAUC<- boxplot(as.matrix(phenotypes[,AUC]),na.rm=TRUE)
#standart deviation
sdGmax<-sd(as.matrix(phenotypes[,Gmaxc]),na.rm=TRUE)
sdU8<- mean(apply(phenotypes[,U8c],2, na.rm=TRUE, mean))
sdT10<- mean(apply(phenotypes[,T10c], 2,na.rm=TRUE, mean))
sdT50<- mean(apply(phenotypes[,T50c],2, na.rm=TRUE, mean))
sdAUC<- mean(apply(phenotypes[,AUCc],2, na.rm=TRUE, mean))

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
t.test(phentypes[,1],genotypes[,1], conf.level = 0.95)$p.value
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
plot(-log10(matru)[1,],t='l')


#The Peak_finder
#in:  1234565656765342243122
#uit: 0011111111211110021000
find.peeks <- function(n = c(1,2,3,4,5,6,5,6,5,6,7,6,5,3,4,2,2,4,3,1,2,2), cutoff = 3){
  tfvector <- which(n > cutoff)
}


if(n<cutoff) x=0
# 400 phenotypes in A. Thaliana
# created: 20-02-12
#(c) created by Inne Lemstra

setwd("X:/computational/400")

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

#environment
grep("NaCl", colnames (phenotypes))
grep("Mannitol", colnames(phenotypes))
grep("Cold", colnames(phenotypes))
grep("Heat", colnames(phenotypes))
grep("ABA", colnames(phenotypes))


#eigenschappen
Gmax <-grep("Gmax", colnames(phenotypes), ignore.case = TRUE)
U8<-grep("U8416", colnames(phenotypes), ignore.case = TRUE)
T10<-grep("T10", colnames(phenotypes), ignore.case = TRUE)
T50<-grep("T50", colnames(phenotypes), ignore.case = TRUE)
AUC<-grep("AUC", colnames(phenotypes), ignore.case = TRUE)

#eigenschappen zonder stratification
Gmaxc <-Gmax[-which(grep("Gmax", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
U8c<-U8[-which(grep("U8416", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
T10c<-T10[-which(grep("T10", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
T50c<-T50[-which(grep("T50", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
AUCc<-AUC[-which(grep("AUC", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]

#boxplot
mGmax<- boxplot(as.matrix(phenotypes[,Gmax]),na.rm=TRUE)
mU8<- boxplot(as.matrix(phenotypes[,U8]),na.rm=TRUE)
mT10<- boxplot(as.matrix(phenotypes[,T10]),na.rm=TRUE)
mT50<- boxplot(as.matrix(phenotypes[,T50]),na.rm=TRUE)
mAUC<- boxplot(as.matrix(phenotypes[,AUC]),na.rm=TRUE)
#standart deviation
sdGmax<-sd(as.matrix(phenotypes[,Gmaxc]),na.rm=TRUE)
sdU8<- mean(apply(phenotypes[,U8c],2, na.rm=TRUE, mean))
sdT10<- mean(apply(phenotypes[,T10c], 2,na.rm=TRUE, mean))
sdT50<- mean(apply(phenotypes[,T50c],2, na.rm=TRUE, mean))
sdAUC<- mean(apply(phenotypes[,AUCc],2, na.rm=TRUE, mean))

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


#The Peak_finder
#in:  1234565656765342243122
#uit: 0011111111211110021000
find.peeks <- function(n = c(1,2,3,4,5,6,5,6,5,6,7,6,5,3,4,2,2,4,3,1,2,2), cutoff = 3){
  tfvector <- which(n > cutoff)
}


if(n<cutoff) x=0
x<-matrix(1:5,10,10)
xcuttoff<- replace(x,x<3,0)
xmaxleft<- replace(xcuttoff,xcuttoff!=max(xcuttoff)& xcuttoff>0,1)
peakseq<- replace(xmaxleft, xmaxleft==max(xmaxleft), 2)

matry<-NULL
for(rowy in 1:ncol(norm)){
	vecty<-NULL
	for(peak in 1:nrow(norm)){
		
	


while( x>0){
	
	maxy<-max(x)
	replace(x,x=maxy,2)
	if(x[10,10]){
	stop("klaar")
	}
	}
	





as.matrix(x)




