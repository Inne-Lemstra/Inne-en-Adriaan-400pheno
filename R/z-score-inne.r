#Z-scores
# created: 01-03-12
#(c) created by Inne Lemstra

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