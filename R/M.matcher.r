#(c) Inne
#date created: 8 maart

#een functie die de collums in een matrix met een andere (matrix) kan vergelijken en er een nieuwe matrix van maakt

#Refmatrix is een grote matrix waarin de colnames gevonden moeten worden
#zoekmatrix is een kleinere matrix met colnames die in Refmatrix gevonden moeten worden
#Term is een woord waarmee je de waarden van alle gevonden kollums vervangt



M.matcher<-function(Refmatrix, zoekmatrix, Term){                           #Refmatrix is een grote matrix waarin de colnames gevonden moeten worden
  if(missing(Refmatrix)) stop("ReferentieMatrix is not found")
  if(missing(zoekmatrix)) stop("Vul een matrix in met zoektermen")          #zoekmatrix is een kleinere matrix met colnames die in Refmatrix gevonden moeten worden
  if(missing(Term)) stop("vul een Term in die de waarden vervangt")         #Term is een woord waarmee je de waarden van alle gevonden kollums vervangt
  Names<-NULL
  matcol<-NULL
  
  for(x in 1:ncol(zoekmatrix)){                                             #het zoeken van de term in alle afzonderlijke matrices
    matchcol<-NULL
    matchcol<-grep(colnames(zoekmatrix)[x],colnames(Refmatrix))             #dit zijn de kollomnummers die een match hebben 
    if(!is.null(matchcol)){                                                 #Maakt een vector met namen
      name<-colnames(Refmatrix)[matchcol]                                   #haalt de huidige naam (van de loop) op 
      Names<-c(Names,name)                                                  #Stopt de huidig naam (van de loop) in een vector
    }
    matcol<-c(matcol,matchcol)                                              #maakt een vector met nummers van de matches
  }
  vec<- 1:nrow(Refmatrix)                                                   #het hernoemen van alle waarden in een kollom
  vec[1:nrow(Refmatrix)]<-Term
  Refmatrix[,matcol] <- vec
  Refmatrix                                                                 #de uiteindelijke matrix met de kollom waarop gezocht moest worden vervangen door de opgegeven term
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





