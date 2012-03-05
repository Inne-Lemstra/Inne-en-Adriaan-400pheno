#Het Groeperen van omgevingen
#	Dacht het nodig te hebben voor Z-scores
#created: 01-03-12
#(c) created by Inne Lemstra

# Term= de term waarop je wilt zoeken(niet hoofletter gevoelig)
# Funk= de functie waarop je wilt zoeken
# DATA = de dataset waarin gezocht moet worden

Zoek <- function(Term, Funk=missing, DATA=colnames(data),Raw=missing){
	T1<-grep(Term, DATA,ignore.case=TRUE)
	if(!missing(Funk)){
		Funk(apply(DATA[,T1], 2, na.rm=TRUE, Funk))
	}
	else{
	if(missing(Raw)){DATA[grep(Term, DATA,ignore.case=TRUE),]
		}else{grep(Term, DATA,ignore.case=TRUE)
		}
	}
}

#Als er geen Funk in wordt gevoerd worden de colnummers weergegeven
#Is alleen getest op standart deviantion en mean
#Bevat nog de stratification waarde

Zoek <- function(Term, DATA, FUN, colTODO = c(1,2,3), index.return = TRUE){
    if(missing(Term)) stop("You need to fill in a term")
    if(missing(DATA)) stop("You need to fill DATA")
	if(!is.matrix(DATA)) stop("DATA needs to be a matrix")
	
	if(!missing(FUN)){
	    if(length(colnames(DATA))==0) stop("You'll need to supply column names")
		T1<-grep(Term, colnames(DATA),ignore.case=TRUE)
		return(FUN(apply(DATA[,T1], 2, na.rm=TRUE, FUN)))
	}else{
	    if(length(rownames(DATA))==0) stop("You'll need to supply row names")
		if(index.return){
		    T1 <- grep(Term, rownames(DATA),ignore.case=TRUE)
			return(DATA[T1,])
		}
		return(grep(Term, rownames(DATA),ignore.case=TRUE))
	}
}


#reference material
#eigenschappen
#Gmax 	<-	grep("Gmax", colnames(phenotypes), ignore.case = TRUE)
#U8		<-	grep("U8416", colnames(phenotypes), ignore.case = TRUE)
#T10		<-	grep("T10", colnames(phenotypes), ignore.case = TRUE)
#T50		<-	grep("T50", colnames(phenotypes), ignore.case = TRUE)
#AUC		<-	grep("AUC", colnames(phenotypes), ignore.case = TRUE)

#eigenschappen zonder stratification
#Gmaxc 	<-	Gmax[-which(grep("Gmax", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
#U8c		<-	U8[-which(grep("U8416", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
#T10c	<-	T10[-which(grep("T10", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
#T50c	<-	T50[-which(grep("T50", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
#AUCc	<-	AUC[-which(grep("AUC", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]

#boxplot
#bGmax	<- 	boxplot(as.matrix(phenotypes[,Gmax]),na.rm=TRUE)
#bU8		<- 	boxplot(as.matrix(phenotypes[,U8]),na.rm=TRUE)
#bT10	<- 	boxplot(as.matrix(phenotypes[,T10]),na.rm=TRUE)
#bT50	<- 	boxplot(as.matrix(phenotypes[,T50]),na.rm=TRUE)
#bAUC	<-	boxplot(as.matrix(phenotypes[,AUC]),na.rm=TRUE)

#mean van means
#mGmax	<-  mean(apply(phenotypes[,Gmaxc], 2, na.rm=TRUE, mean))
#mdU8	<- 	mean(apply(phenotypes[,U8c], 2, na.rm=TRUE, mean))
#mdT10	<- 	mean(apply(phenotypes[,T10c], 2,na.rm=TRUE, mean))
#mdT50	<- 	mean(apply(phenotypes[,T50c], 2, na.rm=TRUE, mean))
#mdAUC	<- 	mean(apply(phenotypes[,AUCc], 2, na.rm=TRUE, mean))

#standart deviation
#sdGmax 	<- 	sd(apply(as.matrix(phenotypes[,Gmaxc]),2, na.rm=TRUE, sd))
#sdU8	<- 	sd(apply(as.matrix(phenotypes[,U8c]),2, na.rm=TRUE, sd))
#sdT10	<- 	sd(apply(as.matrix(phenotypes[,T10c]),2, na.rm=TRUE, sd))
#sdT50	<- 	sd(apply(as.matrix(phenotypes[,T50c]),2, na.rm=TRUE, sd))
#sdAUC	<- 	sd(apply(as.matrix(phenotypes[,AUCc]),2, na.rm=TRUE, sd))

#environment
#grep("NaCl", colnames (phenotypes))
#grep("Mannitol", colnames(phenotypes))
#grep("Cold", colnames(phenotypes))
#grep("Heat", colnames(phenotypes))
#grep("ABA", colnames(phenotypes))
