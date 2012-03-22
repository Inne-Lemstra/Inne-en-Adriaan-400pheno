## copyright Adriaan van der Graaf 2012
# Z-score function



#input een vector 
z.vector     <- function(a){
  if(!is.vector(a)) stop("input has to be a vector for Z")
  vecmean <- mean(a,na.rm=T)
  vecz <- NULL
  for(i in 1:length(a)){vecz <- c(vecz, ((a[i]-vecmean)/sd(a,na.rm=T)))} #hier wordt de z score uitgerekend voor elke waarde in een vector. en in de vector vecz gestopt
  vecz
}
# 