#t.test doen en P-values uitzetten
# created: 01-03-12
#(c) created by Inne Lemstra

Peas<- function(MatrixTrait,MatrixGeno,Clevel){
phenotypes <- MatrixTrait 
genotypes <- MatrixGeno
C <- Clevel											#confidence level

matru<-NULL
for(rmark in 1:ncol(genotypes)){
	vectu<-NULL
	for(b in 1:ncol(phenotypes)){
	    genoA <- which(genotypes[,rmark]=="AA")		#AA genotype selecteren uit col marker
	    genoB <- which(genotypes[,rmark]=="BB")		#BB genotype selecteren uit col marker
		coru <- t.test(phenotypes[genoA,b],phenotypes[genoB,b], conf.level = C, use="pair")$p.value
		vectu<-c(vectu,coru)
	}
	
	matru<- cbind(matru,vectu)
}

	colnames(matru)<- colnames(genotypes)
	Result <- -log10(matru)
}

#om te gebruiken store Peas in een variabele
#vb.	T1 <- Peas(phenotypes, genotypes, 0.95)
