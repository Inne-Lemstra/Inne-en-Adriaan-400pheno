#(c)Inne
#date created:8 maart

#een plotje
chromos<-data[1,405:ncol(data)]
Morgan<-data[2,405:ncol(data)]
#plot grote

X.maker<- function(NameFile,aantalchr){
if(missing(NameFile)) stop("insert file in wich ")
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


Y.maker<-function(Matrix,col.mark,col.waarden, X.as,Fun=NULL){
  if(missing(Matrix)) stop("Voer een Matrix in")
  if(missing(col.mark)) stop("geen kollom met markers gevonden")
  if(missing(col.waarden)) stop("er is geen kollom met waarden om te plotten")
  if(missing(X.as)) stop("geen X.as ingevoerd, draai de X.maker om een x.as te maken")
  
  yass<-NULL
for(y in 1:length(X.as)){
  positie<-which(X.as[y]==Matrix[,col.mark])
    if((!is.na(positie&&1))&(is.null(Fun))){
    waarde<-as.numeric(as.character(unlist(Matrix[positie,col.waarden])),na.rm=TRUE)
    yass<-c(yass, waarde)
    }
    if((!is.na(positie&&1))&(!is.null(Fun))){
    waarde<-Fun(as.numeric(as.character(unlist(Matrix[positie,col.waarden]))),na.rm=TRUE)
    yass<-c(yass, waarde)
    }
    if(is.na(positie&&1)){
    yass<-c(yass,0)
  }
}
  yass
}




#centimorgans
Morgan<-data[2,405:ncol(data)]
#zo moet ik dit doen, van morgan een lijst maken met centimorgans erin.(ten opzichte van elkaar).
#dan in de axis(1, morgan, labels), en in de plot(c(1,de max Morgan),c(y-as)
#de X.maker kan hiervoor omgeschreven worden


  ### here is danny


plotDanny <- function(Morgan, chromos, yass, gapsize=25,type='o'){
  op <- par(las = 2)
  op <- par(cex.axis = 0.6)
  distances <- as.numeric(t(Morgan[1,]))
  chr <- as.numeric(unlist(t(chromos)))
  nchr <-length(unique(chr))
  plot(c(0, getCD(nchr, gapsize=gapsize,distances=distances,chr=chr)),c(0,max(yass)*1.25),t='n',main="Summarized QTL plot", ylab="# of significant QTL", xlab="Markers",xaxt="n")
  par(new=T)
  locs <- NULL
  for(x in 1:nchr){
    locs <- c(locs,distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)))
    points(x=distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)),y=yass[which(chr==x)],t=type,col=x,lwd=3)
  }
  axis(1,locs,labels=names(Morgan[1,]))
}


#2 axis
yass1<-Y.maker(CombiMatrix,2,3:4,xass,mean)
yass2<-Y.maker(CombiMatrix,2,5,xass,mean)


plotInne <- function(Morgan, chromos, yass1, yass2=NULL, gapsize=25,type='l',cuttoff=NULL,Title="Summarized QTL plot",Title_Y.as1="value of significant QTL",Title_Y.as2=""){
if(missing(Morgan)) stop(cat("I am going to stop calling you a white man and I'm going to ask you to stop calling me a black man.","\n", "Morgan afstanden missen","\t"))
if(missing(chromos)) stop("lijst met markers gekoppeld aan op welk chromosoom ze liggen mist")

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
  op <- par(las = 2)
  op <- par(cex.axis = 1)
  distances <- as.numeric(t(Morgan[1,]))
  chr <- as.numeric(unlist(t(chromos)))
  nchr <-length(unique(chr))
  plot(c(0, getCD(nchr, gapsize=gapsize,distances=distances,chr=chr)),c(0,max(yass1)*1.25),type="n",main=Title, ylab=Title_Y.as1, xlab="Markers",xaxt="n")
  axis(2, c(0,max(yass1)*1.25))
  locs <- NULL
  for(x in 1:nchr){
    locs <- c(locs,distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)))
    points(x=distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)),y=yass1[which(chr==x)],type=type,col=x,lwd=3)
  }
  abline(h=cuttoff, lty="dashed")
   if(!is.null(yass2)){
      par(new=T)
    plot(c(0, getCD(nchr, gapsize=gapsize,distances=distances,chr=chr)),c((min(yass2)*1.25),max(yass2)*1.25),type="n", axes=F,,xlab="Markers", ylab=Title_Y.as2)
  axis(4, c((min(yass2)*1.25),max(yass2)*1.25),lwd=1,at=c(-1,1),labels=c("B","A"))
  locs<-NULL
  for(x in 1:nchr){
    
    locs <- c(locs,distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)))
      points(x=distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)),y=yass2[which(chr==x)],t="p",pch=16,col="purple",lwd=3)
   
   }
  }
  axis(1,locs,labels=names(Morgan[1,]))
}
plotInne(Morgan,chromos,yass1,sign(yass2),cuttoff=3)
