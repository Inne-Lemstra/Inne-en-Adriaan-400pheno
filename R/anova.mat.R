##Copyright Adriaan van der Graaf
#anova matrix maken per genotype.

anova.mat <- function(pheno, geno, npheno = 1:ncol(pheno), ngeno= 1:ncol(geno)) {#de matrices met phenotypen en genotypen. en de lengte van de uit te rekenen matrix.
  if(missing(pheno)) stop("matrix pheno is required") #hier het begin van de controle
  if(missing(geno)) stop("matrix geno is required")
  if(nrow(geno) != nrow(pheno)) stop("pheno and geno need the same individuals (rows)")
  if(is.matrix(npheno)) stop("npheno moet een vector van 1 of meer waarden zijn.")
  if(is.matrix(ngeno)) stop("ngeno moet een vector van 1 of meer waarden zijn.")
  #einde controlestap
  #begin definitie voor het uitrekenen
anovamat <- matrix(0,length(npheno),length(ngeno))

for (i in npheno){
  for (n in ngeno){
  anovamat[i,n] <- anova(lm(pheno[,i]~as.factor(geno[,n])))$P[1]  #hier de pwaarden uit de anova krijgen.
  } 
}
colnames(anovamat) <- colnames(geno)[ngeno]
rownames(anovamat) <- colnames(pheno)[npheno] 
  return(-log10(anovamat))
}