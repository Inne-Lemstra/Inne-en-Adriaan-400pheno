#(c) Inne
#date created: 8 maart

#een functie die een collum in een matrix met een andere kan vergelijke en er een nieuwe matrix van maakt

#Refmatrix is een grote matrix waarin de colnames gevonden moeten worden
#zoekmatrix is een kleinere matrix met colnames die in Refmatrix gevonden moeten worden
#Term is een woord waarmee je de waarden van alle gevonden kollums vervangt

M.matcher<-function(Refmatrix, zoekmatrix, Term){
  if(missing(Refmatrix)) stop("ReferentieMatrix is not found")
  if(missing(zoekmatrix)) stop("Vul een matrix in met zoektermen")
  if(ncol(zoekmatrix)>ncol(Refmatrix)) stop("Refmatrix moet groter zijn de zoekmatrix")
  if(missing(Term)) stop("vul een Term in die de waarden vervangt")
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

#Test
#matrixA<-grep.term.col(phenotypes,"NaCl")
#data <- read.csv("BayShatraitsAll.csv",sep=";")
#phenotypes <- data[,1:404]
#T1<-M.matcher(phenotypes,matrixA, "ABA")

#extra
#T1<-matrix(matcol,1,length(matcol))
#colnames(T1)<-Names

#T2<- as.matrix(matcol)
#colnames(T2)<-Names




