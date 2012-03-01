#Matrix met correlatie waarden tussen phenotypes en markers
# created: 01-03-12
#(c) created by Inne Lemstra
matri<-NULL
for(romark in 1:ncol(genotypes)){
	
	vecto<- NULL
	for(a in 1:ncol(phenotypes)){
		
		core<-cor(phenotypes[,a],as.numeric(genotypes[,romark]), use="pair")
		vecto<-c(vecto,core)
	}
	
	matri<- cbind(matri, vecto )
}

abovethreshold <- apply(matri>0.2,2,function(x){which(x)})
nietNULL <- unlist(lapply(abovethreshold,function(x){!(is.na(x)&&1)}))
abovethreshold[nietNULL]


 colnames(matri) <- colnames(genotypes)
apply(matri, 2, function(x) (x>0.8))

# visualiseren matri
op <- par(las = 2)
op<- par(cex = 0.6)
op <- par(mai=c(3,0.4,0,0))

heatmap(matri)
boxplot(matri, las=2, cex=0.6, mai=c(3,0.4,0,0) )

#Dit werkt om een of andere reden niet meer