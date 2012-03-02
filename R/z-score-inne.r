#Z-scores
# created: 01-03-12
#(c) created by Inne Lemstra

#Z-scores bepalen voor een aantal kollommen
#colgroups zijn de nummers van de kollommen
#DATA is de dataset waaruit de waardes gehaalt moeten worden

Zmat<-function(colgroups,DATA){

matr <- NULL
for(pheno in colgroups){
	gemm <-mean(DATA[,pheno], na.rm=TRUE)
	stand<- sd(DATA[,pheno], na.rm=TRUE)
	vect <- NULL
	for(a in 1:nrow(DATA)){
		z<- ((DATA[a,pheno]-gemm)/stand)
		vect <- c(vect,z)
	}
	matr <- cbind(matr,vect)
}
	colnames(matr) <- colnames(phenotypes)
	matr
	
}	

#Ik wil er ook graag nog in hebben dat als je bij colgroups 
# een character invoerd hij gaat zoeken in de namen van kollomen en vervolgens die nummers gebruikt
