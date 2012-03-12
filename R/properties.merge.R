##Copyright Adriaan van der Graaf
#nieuwe functie om per property te bepaalde dingen te identificeren.

properties.merge <- function(traitmat,propvec){ #in deze functie word ervan uit gegaan dat de traitmat een matrix is met de eerste kolommen resp. trait en marker zijn.
  if (missing(traitmat)) stop("voer een traitmat in")
  if (missing(proplist)) stop("voer een propmat in") 

  traitprops <- vector("list",length(propvec)) 
  grepvar <- vector("list",length(propvec)) 
  MultiAnova <- (1:nrow(traitmat))*NA
  nondouble <- NULL
  for (i in 1:length(propvec)){
	grepvar [[i]] <- grep(names(propvec)[[i]], traitmat[,1],fixed=T)
	  for (n in 1: length(propvec[[i]])){
	   doubles <- grep(names(propvec[[i]][n]), traitmat[grepvar[[i]],2])
	   MultiAnova[grepvar[[i]][doubles]] <- propvec[[i]][n]	  
	   if(length(doubles) ==  0)  {nondouble <- c(nondouble,names(propvec)[i],names(propvec[[i]])[n],as.numeric(propvec[[i]][n])) }
	  }
  }
traitmattest <- matrix(NA,(length(nondouble)/3),7)
traitmattest[,1] <- nondouble[seq(1,length(nondouble),3)]
traitmattest[,2] <- nondouble[seq(2,length(nondouble),3)]
traitmattest[,7] <- nondouble[seq(3,length(nondouble),3)]
colnames(traitmattest) <- colnames(traitmat)
traitmat <- (cbind(traitmat,MultiAnova))
traitmat <- rbind(traitmat,traitmattest)
traitmat
}
