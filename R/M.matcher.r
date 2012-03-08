#(c) Inne
#date created: 8 maart

#een functie die een collum in een matrix met een andere kan vergelijke en er een nieuwe matrix van maakt

matrixA<-grep.term.col(phenotypes,"ABA")
Refmatrix<-phenotypes

M.matcher<-function(Refmatrix, zoekmatrix, Term){
Names<-NULL
matcol<-NULL
for(x in 1:ncol(zoekmatrix)){
matchcol<-NULL
matchcol<-grep(colnames(zoekmatrix)[x],colnames(Refmatrix))
if(!is.null(matchcol)){
name<-colnames(Refmatrix)[matchcol]
Names<-c(Names,name)
}
matcol<-c(matcol,matchcol)
}
vec<- 1:nrow(Refmatrix)
vec[1:nrow(Refmatrix)]<-Term
Refmatrix[,matcol] <- vec
Refmatrix
}


T1<-matrix(matcol,1,length(matcol))
colnames(T1)<-Names

T2<- as.matrix(matcol)
colnames(T2)<-Names


batchmat<- replace(matrixB[,matcol]

