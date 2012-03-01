#Het Groeperen van omgevingen
#	Dacht het nodig te hebben voor Z-scores
#created: 01-03-12
#(c) created by Inne Lemstra

#eigenschappen
Gmax 	<-	grep("Gmax", colnames(phenotypes), ignore.case = TRUE)
U8		<-	grep("U8416", colnames(phenotypes), ignore.case = TRUE)
T10		<-	grep("T10", colnames(phenotypes), ignore.case = TRUE)
T50		<-	grep("T50", colnames(phenotypes), ignore.case = TRUE)
AUC		<-	grep("AUC", colnames(phenotypes), ignore.case = TRUE)

#eigenschappen zonder stratification
Gmaxc 	<-	Gmax[-which(grep("Gmax", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
U8c		<-	U8[-which(grep("U8416", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
T10c	<-	T10[-which(grep("T10", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
T50c	<-	T50[-which(grep("T50", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]
AUCc	<-	AUC[-which(grep("AUC", colnames(phenotypes), ignore.case = TRUE) %in% grep("Stratification", colnames(phenotypes), ignore.case=TRUE))]

#boxplot
bGmax	<- 	boxplot(as.matrix(phenotypes[,Gmax]),na.rm=TRUE)
bU8		<- 	boxplot(as.matrix(phenotypes[,U8]),na.rm=TRUE)
bT10	<- 	boxplot(as.matrix(phenotypes[,T10]),na.rm=TRUE)
bT50	<- 	boxplot(as.matrix(phenotypes[,T50]),na.rm=TRUE)
bAUC	<-	boxplot(as.matrix(phenotypes[,AUC]),na.rm=TRUE)

#mean van means
mGmax	<-  mean(apply(phenotypes[,Gmaxc], 2, na.rm=TRUE, mean))
mdU8	<- 	mean(apply(phenotypes[,U8c], 2, na.rm=TRUE, mean))
mdT10	<- 	mean(apply(phenotypes[,T10c], 2,na.rm=TRUE, mean))
mdT50	<- 	mean(apply(phenotypes[,T50c], 2, na.rm=TRUE, mean))
mdAUC	<- 	mean(apply(phenotypes[,AUCc], 2, na.rm=TRUE, mean))

#standart deviation
sdGmax 	<- 	sd(apply(as.matrix(phenotypes[,Gmaxc]),2, na.rm=TRUE, sd))
sdU8	<- 	sd(apply(as.matrix(phenotypes[,U8c]),2, na.rm=TRUE, sd))
sdT10	<- 	sd(apply(as.matrix(phenotypes[,T10c]),2, na.rm=TRUE, sd))
sdT50	<- 	sd(apply(as.matrix(phenotypes[,T50c]),2, na.rm=TRUE, sd))
sdAUC	<- 	sd(apply(as.matrix(phenotypes[,AUCc]),2, na.rm=TRUE, sd))

