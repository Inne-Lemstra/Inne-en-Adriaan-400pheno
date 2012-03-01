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
plot(-log10(matru)[1,],t='l')