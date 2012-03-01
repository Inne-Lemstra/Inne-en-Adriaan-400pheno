#t.test doen
# created: 01-03-12
#(c) created by Inne Lemstra

matru<-NULL
for(rmark in 1:ncol(genotypes)){
	vectu<-NULL
	for(b in 1:ncol(phenotypes)){
	    genoA <- which(genotypes[,rmark]=="AA")		#AA genotype selecteren uit col marker
	    genoB <- which(genotypes[,rmark]=="BB")		#BB genotype selecteren uit col marker
		coru <- t.test(phenotypes[genoA,b],phenotypes[genoB,b], conf.level = 0.95, use="pair")$p.value
		vectu<-c(vectu,coru)
	}
	
	matru<- cbind(matru,vectu)
}

colnames(matru)<- colnames(genotypes)
plot(-log10(matru)[1,],t='l')