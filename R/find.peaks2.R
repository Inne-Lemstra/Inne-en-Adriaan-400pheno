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


x<-matrix(1:6,10,10)
xcuttoff<- replace(x,x<3,0)
xmaxleft<- replace(xcuttoff,xcuttoff!=max(xcuttoff)& xcuttoff>0,1)
peakseq<- replace(xmaxleft, xmaxleft==max(xmaxleft), 2)


x<-matrix(1:6,10,10)

vecty<-NULL
xcuttoff<- replace(x,x<3,0)
	for(sequ in 1:ncol(x)){
        
		minivector<-NULL
		while(xcuttoff>0){
			miniwaarde<-xcuttoff
			minivector<-c(minivector,miniwaarde)
		}
		peaks<-replace(minivector, minivector==max(minivector), 2)
		complete<- replace(peaks, peaks!=2, 1)
		
		
		

	}
	
peaks <- function(a,cutoff){
    sequ <- (a >= cutoff)
 	index_counter <- 1
	s_index <- 1
	e_index <- 1
	found_true <- FALSE
	result <- NULL
	for(x in sequ){
	 cat(index_counter,x,found_true,"\n")
	 #We komen een true tegen vanuit 0
	 if(x && !found_true){
	   s_index <- index_counter
	   found_true <- TRUE
	 }
	 #plak de 0 in de 0-regio aan het resultaat
	 if(!x && !found_true) result <- c(result,0)
	 #We komen een FALSE tegen vanuit T
	 if(!x && found_true){
	   e_index <- index_counter
	   found_true <- FALSE
	   maxx <- which.max(a[s_index:e_index])
	   temp <- as.numeric(sequ[s_index:e_index])
	   temp[maxx] <- 2
	   result <- c(result,temp)
	 }

	 index_counter <- index_counter + 1
	}
	#Einde we kunnen nog een stukje niet gedaan hebben
	if(found_true){
	   e_index <- length(sequ)
	   maxx <- which.max(a[s_index:e_index])
	   temp <- as.numeric(sequ[s_index:e_index])
	   temp[maxx] <- 2
	   result <- c(result,temp)
	}
	result
}
	peaks(x[,1],3)

    #which (a > cutoff)
    #0 0 T T T T 0 0 T T 0
    #welke index is true (s_index) en welke daarna is 0 (e_index)
    #3 4 5 6	
	#4 <- which.max()
	#0 0 T T T 2 0 0