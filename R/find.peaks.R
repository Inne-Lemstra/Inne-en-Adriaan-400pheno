# Peak finding in R
# (c) 2012 By Inne
# 
# Idea behind this functions: find peaks and annotate them
#in:  1234565656765342243122
#out: 0011111111211110021000

find.peaks <- function(n = c(1,2,3,4,5,6,5,6,5,6,7,6,5,3,4,2,2,4,3,1,2,2), cutoff = 3){
  tfvector <- which(n > cutoff)
}

test_find.peaks(){
  if(find.peaks() != c(0,0,1,1,1,1,1,1,1,1,2,1,1,1,1,0,0,2,1,0,0,0)){
    stop("Test failed !")
  }
}
