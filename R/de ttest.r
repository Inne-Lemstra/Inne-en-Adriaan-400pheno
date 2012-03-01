#t.test doen en P-values uitzetten
# created: 01-03-12
#(c) created by Inne Lemstra

Peas<- function(MatrixTrait, MatrixGeno, conf.level = 0.95){
  matru<-NULL
  for(rmark in 1:ncol(MatrixGeno)){
    vectu<-NULL
    for(b in 1:ncol(MatrixTrait)){
	    genoA <- which(MatrixGeno[,rmark]=="AA")		#AA genotype selecteren uit col marker
	    genoB <- which(MatrixGeno[,rmark]=="BB")		#BB genotype selecteren uit col marker
      coru <- t.test(MatrixTrait[genoA,b],MatrixTrait[genoB,b], conf.level = conf.level, use="pair")$p.value
      vectu<-c(vectu,coru)
    }
    matru<- cbind(matru,vectu)
  }

	colnames(matru)<- colnames(MatrixGeno)
	return(-log10(matru))
}

#om te gebruiken store Peas in een variabele
#vb.	T1 <- Peas(phenotypes, genotypes, 0.95)
