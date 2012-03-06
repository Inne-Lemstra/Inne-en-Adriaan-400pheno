#t.test doen en P-values uitzetten
# created: 01-03-12
#(c) created by Inne Lemstra

t.test.mat<- function(MatrixTrait, MatrixGeno, geno1,geno2, conf.level){
  if(length(geno1) != 1) stop("marker definition 1 needs to be of length 1")
  if(length(geno2) != 1) stop("marker definition 2 needs to be of length 1")
  if(length(conf.level) != 1) stop("confidence level needs to be assigned")
  matru<-NULL
  for(rmark in 1:ncol(MatrixGeno)){
    vectu<-NULL
    for(b in 1:ncol(MatrixTrait)){
	    genoA <- which(MatrixGeno[,rmark]==geno1)		#AA genotype selecteren uit col marker
	    genoB <- which(MatrixGeno[,rmark]==geno2)		#BB genotype selecteren uit col marker
      coru <- t.test(MatrixTrait[genoA,b],MatrixTrait[genoB,b], conf.level = conf.level, use="pair")$p.value
      vectu<-c(vectu,coru)
    }
    matru<- cbind(matru,vectu)
  }

	colnames(matru)<- colnames(MatrixGeno)
	rownames(matru) <- colnames(MatrixTrait)
	return(-log10(matru)) #levert de min 10 log van de p waarde.
}

#om te gebruiken store t.test.mat in een variabele
#vb.	T1 <- t.test.mat(phenotypes, genotypes, 0.95)
#vb.	T1 <- t.test.mat(phenotypes, genotypes, c("CC","DD"), 0.95)
