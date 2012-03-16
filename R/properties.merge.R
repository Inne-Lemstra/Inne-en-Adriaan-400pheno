##Copyright Adriaan van der Graaf
#nieuwe functie om per property te bepaalde dingen te identificeren.

properties.merge <- function(traitmat,propvec){ #in deze functie word ervan uit gegaan dat de traitmat een matrix is met de eerste kolommen resp. trait en marker zijn.
  if (missing(traitmat)) stop("voer een traitmat in") #controle als er iets mist.
  if (missing(propvec)) stop("voer een propmat in") 

 #hier definieren van functies. grepvar, zijn de variabeelen in de traitmat die de namen van de list van propvec volgen
  grepvar <- vector("list",length(propvec)) 
  MultiAnova <- (1:nrow(traitmat))*NA #de vector die aan de traitmat wordt gecbind.
  nondouble <- NULL #dit worden de markers die onder de traitmat worden gerbind. omdat ze niet voorkomen in de traitmat, maar wel in de propvec.
  for (i in 1:length(propvec)){
	grepvar [[i]] <- grep(names(propvec)[[i]], traitmat[,1],fixed=T) #welke traits hebben de namen van de properties.
	  for (n in 1: length(propvec[[i]])){
	   doubles <- grep(names(propvec[[i]][n]), traitmat[grepvar[[i]],2]) #hier worden de markers vergeleken met de markers in propvec.
	   MultiAnova[grepvar[[i]][doubles]] <- propvec[[i]][n]	   #hier worden de marker-waarden in de MultiAnova vector gezet op de plaats van de marker.
	   if(length(doubles) ==  0)  {nondouble <- c(nondouble,names(propvec)[i],names(propvec[[i]])[n],as.numeric(propvec[[i]][n])) } #als er geen dubbelen zijn, dan komt de marker met property en value in nondouble gezet. 
	  }
  }

traitmat <- cbind(traitmat,MultiAnova) #hier de multianova aan de traitmat cbinden. Dat wordt hier gedaan omdat de colnames van hierboven nog hetzelfde zijn
traitmattest <- matrix(NA,(length(nondouble)/3),7) #traitmattest is voor de non-doubles.
traitmattest[,1] <- nondouble[seq(1,length(nondouble),3)] #hier de properties in de traitmattest
traitmattest[,2] <- nondouble[seq(2,length(nondouble),3)]#hier de marker
traitmattest[,7] <- nondouble[seq(3,length(nondouble),3)]#hier de value
 #colnames samenvoegen, anders werkt Rbind niet.
colnames(traitmattest) <- colnames(traitmat)
traitmat <- rbind(traitmat,traitmattest,deparse.level = 0) #hier de nondoubles aan de traitmat rbinden


return(traitmat) #hier traitmat tonen.
}
