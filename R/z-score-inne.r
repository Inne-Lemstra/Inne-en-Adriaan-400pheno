#Z-scores
# created: 01-03-12
#(c) created by Inne Lemstra

#Z-scores bepalen voor een aantal kollommen
#colgroups zijn de nummers van de kollommen
#DATA is de dataset waaruit de waardes gehaalt moeten worden

Zmat<-function(colgroups,DATA){

matr <- NULL
for(pheno in colgroups){                                #voor alle kollomen
	gemm <-mean(DATA[,pheno], na.rm=TRUE)                 #mean bepalen
	stand<- sd(DATA[,pheno], na.rm=TRUE)                  #Standart deviation bepalen
	vect <- NULL
	for(a in 1:nrow(DATA)){                               #z-score per individu uitrekenen
		z<- ((DATA[a,pheno]-gemm)/stand)
		vect <- c(vect,z)                                   #z-score in een vector stoppen
	}
	matr <- cbind(matr,vect)                              #alle z-score (per kollom) samenvoegen
}
	colnames(matr) <- colnames(phenotypes)                #Kollom namen gelijk zetten
	matr                                                  #display matrix
	
}

#Ik wil er ook graag nog in hebben dat als je bij colgroups 
# een character invoerd hij gaat zoeken in de namen van kollomen en vervolgens die nummers gebruikt
