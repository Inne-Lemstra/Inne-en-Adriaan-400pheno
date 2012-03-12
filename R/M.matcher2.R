#(c) Inne
#date created: 8 maart

#een functie die een collum in een matrix met een andere kan vergelijke en er een nieuwe matrix van maakt

matrixA<-grep.term.col(phenotypes,"ABA")
Refmatrix<-phenotypes

M.matcher<-function(Refmatrix, zoekmatrix, Term, addmatrix){
  if(missing(addmatrix)) addmatrix <- Refmatrix*NA
  if(dim(addmatrix) != dim(Refmatrix)) addmatrix <- Refmatrix*NA
  Names<-NULL
  matcol<-NULL
    for(x in 1:ncol(zoekmatrix)){
      matchcol<-NULL
      matchcol<-grep(colnames(zoekmatrix)[x],colnames(Refmatrix))
        if(!is.null(matchcol)){
           Name<-colnames(Refmatrix)[matchcol]
           Names<-c(Names,Name)
        }
      matcol<-c(matcol,matchcol)
    }
  vec<- 1:nrow(addmatrix)
  vec[1:nrow(addmatrix)]<-Term
  addmatrix[,matcol] <- vec
  return(addmatrix)
}
