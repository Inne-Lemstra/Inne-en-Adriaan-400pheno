#(c)Inne
#date created:8 maart

#een plotje

#plot grote
plot(c(0,10),c(0,10),type="n")
plot(c(0,length(xass)),c(1,107),type="n", xlab="Markers",ylab="Number of LOD above cuttof",axes=FALSE)

#bekijk ?axis
op <- par(las = 2)
op<- par(cex = 0.6)
op <- par(mai=c(1,0.8,1,0.5))

axis(2,0:107)
X.maker(chromos,5,2)  
points(yass)

X.maker<- function(NameFile,aantalchr,gap){
xass<-NULL
for(x in 1:aantalchr){
  if(x!=aantalchr){
    vec<-NULL
    vec<-colnames(NameFile)[which(NameFile==x)]
    vec<-c(vec,rep(" ",gap))
  }
    else{
    vec<-NULL
    vec<-colnames(NameFile)[which(NameFile==x)]
    }
  xass<- c(xass,vec)
  }
  axis(1,1:length(xass), labels=xass)
  }

X.maker(chromos,5,2)  
#nu de points invullen
table(CombiMatrix[,2])

yass<-NULL
for(y in 1:length(xass)){
  positie<-NULL
  positie<-grep(xass[y],names(table(CombiMatrix[,2])))
    if(!is.na(positie&&1)){
    yass<-c(yass, table(CombiMatrix[,2])[positie])
  }
  else{
    yass<-c(yass,0)
  }
  }


