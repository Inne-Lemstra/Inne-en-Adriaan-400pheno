#(c)Inne
#date created:8 maart

#een plotje

#plot grote
plot(c(0,10),c(0,10),type="n")
plot(c(100,600),c(1,107),type="n", xlab="Markers",ylab="Number of LOD above cuttof",axes=FALSE)

#bekijk ?axis
op <- par(las = 2)
op<- par(cex = 0.6)
op<- par(mai=c(1,0.6,1,1))

axis(1,vecB,labels=xass[1:length(xass)])
axis(2,0:107)
X.maker(chromos,5,2)
points(vecB,yass, t="l")

X.maker<- function(NameFile,aantalchr){
xass<-NULL
for(x in 1:aantalchr){
  #if(x!=aantalchr){
   # vec<-NULL
    #vec<-colnames(NameFile)[which(NameFile==x)]
    #vec<-c(vec," ")
  #}
    #else{
    vec<-NULL
    vec<-colnames(NameFile)[which(NameFile==x)]
    #}
  xass<- c(xass,vec)
  }
  axis(1,1:length(xass), labels=xass)
  xass
  }

xass<-X.maker(chromos,5) 
axis(1,1:length(xass), labels=xass)
#nu de points invullen
table(CombiMatrix[,2])

yass<-NULL
for(y in 1:length(xass)){
  positie<-NULL
  positie<-which(xass[y]==names(table(CombiMatrix[,2])))
    if(!is.na(positie&&1)){
    yass<-c(yass, table(CombiMatrix[,2])[positie])
  }else{
    yass<-c(yass,0)
  }
  cat(y,length(yass),"\n")
}

#centimorgans
Morgan<-data[2,405:ncol(data)]
#zo moet ik dit doen, van morgan een lijst maken met centimorgans erin.(ten opzichte van elkaar).
#dan in de axis(1, morgan, labels), en in de plot(c(1,de max Morgan),c(y-as)
#de X.maker kan hiervoor omgeschreven worden


vecB<-NULL
gap<-10
for(chr in 1:ncol(chromos)){
  #if(as.numeric(as.character(Morgan[1,chr]))==0){vecB<-c(vecB,(vecA+gap))
  vecA<-as.numeric(as.character(Morgan[1,chr]))+(as.numeric(as.character(chromos[1,chr]))*100)+gap
  vecB<- c(vecB, vecA)
  }
