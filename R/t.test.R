# \file t.test.R
# Created: 01-03-12
# (C) created by Inne Lemstra

t.test.mat<- function(MatrixTrait, MatrixGeno, genocode = c("AA","BB"), conf.level = 0.95){
  if(missing(MatrixTrait)) stop("No traits")
  if(missing(MatrixGeno)) stop("No genotypes")
  matru<-NULL
  for(rmark in 1:ncol(MatrixGeno)){
    vectu<-NULL
    for(b in 1:ncol(MatrixTrait)){
      genoA <- which(MatrixGeno[,rmark]==genocode[1])   #AA genotype selecteren uit col marker
      genoB <- which(MatrixGeno[,rmark]==genocode[2])   #BB genotype selecteren uit col marker
      coru <- t.test(MatrixTrait[genoA,b],MatrixTrait[genoB,b], conf.level = conf.level, use="pair")$p.value
      vectu<-c(vectu,coru)
    }
    matru<- cbind(matru,vectu)
  }

  colnames(matru)<- colnames(MatrixGeno)
  rownames(matru) <- colnames(MatrixTrait)
  return(-log10(matru)) #levert de min 10 log van de p waarde.
}

#om te gebruiken store t.test.mat in een variabele
#vb.  T1 <- t.test.mat(phenotypes, genotypes, conf.level = 0.95)
#vb.  T1 <- t.test.mat(phenotypes, genotypes, c("CC","DD"), 0.95)
