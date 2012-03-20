#(c)Inne
#date created:8 maart

#een plotje
chromos<-data[1,405:ncol(data)]
Morgan<-data[2,405:ncol(data)]
#plot grote

#Bepaalt de volgorde van de Xas (is alleen nodig als je ook de Y.maker gaat gebruiken)
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

#Haalt de points uit een matrix (met markers en waarden) en zet deze in volgorde van de Xas
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


#2 axis
yass1<-Y.maker(CombiMatrix,2,3:4,xass,mean)
yass2<-Y.maker(CombiMatrix,2,5,xass,mean)


plotInne <- function(Morgan, chromos, yass1, yass2=NULL, gapsize=25,type='l',cuttoff=NULL,Title="Summarized QTL plot",Title_Y.as1="value of significant QTL",Title_Y.as2=""){
if(missing(Morgan)) stop(cat("I am going to stop calling you a white man and I'm going to ask you to stop calling me a black man.","\n", "Morgan afstanden missen","\t"))
if(missing(chromos)) stop("lijst met markers gekoppeld aan op welk chromosoom ze liggen mist")

#Getd bepaald de centimorgan afstand tussen de markers van één chromosoom
getD <- function(which.chr=1,distances,chr){
  distances[max(which(chr==which.chr))]
}

#GetCD bepaald de chromosoom distance en telt er een gap bij op
getCD <- function(which.chr=1, gapsize = 25,distances,chr){
  if(which.chr==0) return(0)
  D <- 0
  for(x in 1:which.chr){
    D <- D + getD(x,distances,chr) + gapsize
  }
  return(D)
}
#hier worden plot parameters meegegeven
  op <- par(las = 2)                        #las is loodrechte x-aslabel notatie
  op <- par(cex.axis = 1)                   #De grootte van de labels
  distances <- as.numeric(t(Morgan[1,]))    #is de afstand van het totaal
  chr <- as.numeric(unlist(t(chromos)))     #Chr is welk chromosoom en de lengte ervan
  nchr <-length(unique(chr))                #nchr is de lenget van alle chromosomen tot dusver
  plot(c(0, getCD(nchr, gapsize=gapsize,distances=distances,chr=chr)),c(0,max(yass1)*1.25),type="n",main=Title, ylab=Title_Y.as1, xlab="Markers",xaxt="n")        #De algemene plot
  axis(2, c(0,max(yass1)*1.25))             #De Y-as
  locs <- NULL                              #Locaties op de x-as
  for(x in 1:nchr){                         #De loop voor het plotten van je points
    locs <- c(locs,distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)))
    points(x=distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)),y=yass1[which(chr==x)],type=type,col=x,lwd=3)
  }
  abline(h=cuttoff, lty="dashed")           #de cut-off
   
   if(!is.null(yass2)){                     #Bepalen of 2e Yas gewenst is
  
  par(new=T)                                #Tweede Yas
    plot(c(0, getCD(nchr, gapsize=gapsize,distances=distances,chr=chr)),c((min(yass2)*1.25),max(yass2)*1.25),type="n", axes=F,,xlab="Markers", ylab=Title_Y.as2)  #de algemene plot 2e Yas
  axis(4, c((min(yass2)*1.25),max(yass2)*1.25),lwd=1,at=c(-1,0,1),labels=c("B","NA","A"))                                                                         #tweede Yas
  
  locs<-NULL                                #loop om de points te bepalen net als hierboven
  for(x in 1:nchr){
    locs <- c(locs,distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)))
      points(x=distances[which(chr==x)] + (getCD(x-1,gapsize,distances,chr)),y=yass2[which(chr==x)],t="p",pch=16,col="purple",lwd=3)
   }
  
  }
  axis(1,locs,labels=names(Morgan[1,]))     #de x-as notatie
}
plotInne(Morgan,chromos,yass1,sign(yass2),cuttoff=3)
