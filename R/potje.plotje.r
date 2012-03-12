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
axis(2,seq(0,110,10))
X.maker(chromos,5)
#points(vecB,yass, t="b")

#point calculator

for(scheidslijn in 1:6){

Punten <-which(vecB<(scheidslijn*100)&vecB>=(100*scheidslijn-100))
points(vecB[Punten],yass[Punten], t="b")
}



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
#axis(1,1:length(xass), labels=xass)
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
  vecA<-as.numeric(as.character(Morgan[1,chr]))+(as.numeric(as.character(chromos[1,chr]))*100)
  vecB<- c(vecB, vecA)
  }

 

 vecA=NULL
  vecB<-NULL
  z=0
gap<-10
for(chr in 1:ncol(chromos)){
  if(as.numeric(as.character(Morgan[1,chr]))==0){
  vecB<-c(vecB,(vecA+gap))
  z=z+1
  cat(chr,"\n")
  }
    vecA<-as.numeric(as.character(Morgan[1,chr]))+(as.numeric(as.character(chromos[1,chr]))*100)+gap*z
    vecB<- c(vecB, vecA)
  }
  
  
  
  
  
  ### here is danny
  
getD <- function(which.chr=1,distances,chr){
  distances[max(which(chr==which.chr))]
}

getCD <- function(which.chr=1, gapsize = 25,distances,chr){
  if(which.chr==0) return(0)
  D <- 0
  for(x in 1:which.chr){
    D <- D + getD(x,distances,chr) + gapsize
  }
  return(D)
}


plotDanny <- function(Morgan, chromos, yass, gapsize=25,type='o'){
  op <- par(las = 2)
  op <- par(cex.axis = 0.6)
  distances <- as.numeric(t(Morgan[1,]))
  chr <- as.numeric(unlist(t(chromos)))
  nchr <-length(unique(chr))
  plot(c(0, getCD(nchr, gapsize=gapsize,distances=distances,chr=chr)),c(0,max(yass)*1.25),t='n',main="Summarized QTL plot", ylab="# of significant QTL", xlab="Markers",xaxt="n")
  locs <- NULL
  for(x in 1:nchr){
    locs <- c(locs,distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)))
    points(x=distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)),y=yass[which(chr==x)],t=type,col=x,lwd=3)
  }
  axis(1,locs,labels=names(Morgan[1,]))
}

plotDanny(Morgan,chromos,yass,gapsize=50)
plotDanny(Morgan,chromos,yass,gapsize=50,type="h")