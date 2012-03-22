#(c)Inne
# Date created: 21-03-2012

# een functie om een matrix met sequences (AAof BB) in een image te plotten

plotSequence<-function(uitkomst_marker.choice,las=2,cex=0.4,title=NULL){
if(missing(uitkomst_marker.choice)) stop("Draai eerst de Marker.choice en voer waarden hier in")
if(class(uitkomst_marker.choice)!="list"&class(uitkomst_marker.choice)!="matrix") stop("Ingevoerde sequence moet een lijst of een matrix zijn")

  if(class(uitkomst_marker.choice)=="list"){                        #Bepalen of het een lijst of matrix is (voor lijst gelden anderen voorwaarden)
    matri <- NULL
    for(x in uitkomst_marker.choice){matri <- cbind(matri,x)}       #lijst omzetten naar een matrix
    colnames(matri)<-names(uitkomst_marker.choice)                  #namen mee geven
    rownames(matri)<-names(uitkomst_marker.choice[[1]])
    uitkomst_marker.choice <- matri
  }
  #Now uitkomst_marker.choice is a matrix
  SEQ <- uitkomst_marker.choice                                     #ingevoerde matrices starten hier
  SEQ[which(SEQ=="AA")]<-1                                          #A en B omzetten naar getallen
  SEQ[which(SEQ=="BB")]<-2
  SEQ[which(SEQ=="-")]<-3

  SEQ<-apply(SEQ,2,as.numeric)                                      #Alles als numeric inlezen
  if(class(uitkomst_marker.choice)=="list"){rownames(SEQ)<-rownames(matri)}                     #Namen gelijktrekken list
  if(class(uitkomst_marker.choice)=="matrix"){rownames(SEQ)<-rownames(uitkomst_marker.choice)}  #Namen gelijktrekken Matrix

  op<-par(las=las)                                                                              #Een image maken
  op<-par(cex=cex)
  op<-par(mai=c(2,1.25,0.4,0.1))
  image(1:ncol(SEQ),1:nrow(SEQ), t(SEQ),col=c("green","red","white"),xlab="Traits", ylab="Markers",xaxt="n",yaxt="n",main=title)    #kleuren worden met de hand ingevoerd
  axis(1,1:ncol(SEQ), labels=colnames(SEQ))
  axis(2,1:nrow(SEQ), labels=rownames(SEQ))
  legend("bottom", c("AA","BB"),lty=1, lwd=3, col=c("red","green"))                             #kleine Legend toevoegen
  box()
}

