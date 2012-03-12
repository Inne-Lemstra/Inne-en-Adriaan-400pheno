#(c)Inne
#date created:8 maart

#een plotje
chromos<-data[1,405:ncol(data)]
Morgan<-data[2,405:ncol(data)]
#plot grote
plot(c(100,600),c(1,107),type="n", xlab="Markers",ylab="Number of LOD above cuttof",axes=FALSE)

X.maker<- function(NameFile,aantalchr){
xass<-NULL
for(x in 1:aantalchr){
    vec<-NULL
    vec<-colnames(NameFile)[which(NameFile==x)]
  xass<- c(xass,vec)
  }
  xass
  }

xass<-X.maker(chromos,5) 
#nu de points invullen

yass<-NULL
for(y in 1:length(xass)){
  positie<-NULL
  positie<-which(xass[y]==names(table(CombiMatrix[,2])))
    if(!is.na(positie&&1)){
    yass<-c(yass, table(CombiMatrix[,2])[positie])
  }else{
    yass<-c(yass,0)
  }
}

Y.maker<-function(Matrix,col.mark,col.waarden, X.as,Fun){
  yass<-NULL
for(y in 1:length(X.as)){
  positie<-which(X.as[y]==Matrix[,col.mark])
    if(!is.na(positie&&1)){
    waarde<-Fun(as.numeric(Matrix[positie,col.waarden]),na.rm=TRUE)
    yass<-c(yass, waarde)
  }else{
    yass<-c(yass,0)
  }
}
yass
}

#bekijk ?axis
op<- par(las = 2)
op<- par(cex = 0.6)
op<- par(mai=c(1,0.6,1,1))

axis(1,vecB,labels=xass[1:length(xass)])
axis(2,seq(0,110,10))
X.maker(chromos,5)
#points(vecB,yass, t="b")

#centimorgans
Morgan<-data[2,405:ncol(data)]
#zo moet ik dit doen, van morgan een lijst maken met centimorgans erin.(ten opzichte van elkaar).
#dan in de axis(1, morgan, labels), en in de plot(c(1,de max Morgan),c(y-as)
#de X.maker kan hiervoor omgeschreven worden


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