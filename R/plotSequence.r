#(c)Inne
# Date created: 21-03-2012

# een functie om een matrix met sequences (AAof BB) in een image te plotten

plotSequence<-function(uitkomst_marker.choice,las=2,cex=0.4,title=NULL){

  if(class(uitkomst_marker.choice)=="list"){
    matri <- NULL
    for(x in uitkomst_marker.choice){matri <- cbind(matri,x)}
    colnames(matri)<-names(uitkomst_marker.choice)
    rownames(matri)<-names(uitkomst_marker.choice[[1]])
    uitkomst_marker.choice <- matri
  }
  #Now uitkomst_marker.choice is a matrix
  SEQ <- uitkomst_marker.choice
  SEQ[which(SEQ=="AA")]<-1
  SEQ[which(SEQ=="BB")]<-2
  SEQ[which(SEQ=="-")]<-3

  SEQ<-apply(SEQ,2,as.numeric)
  if(class(uitkomst_marker.choice)=="list"){rownames(SEQ)<-rownames(matri)}
  if(class(uitkomst_marker.choice)=="matrix"){rownames(SEQ)<-rownames(uitkomst_marker.choice)}

  op<-par(las=las)
  op<-par(cex=cex)
  op<-par(mai=c(2,1.25,0.4,0.1))
  image(1:ncol(SEQ),1:nrow(SEQ), t(SEQ),col=c("green","red","white"),xlab="Traits", ylab="Markers",xaxt="n",yaxt="n",main=title)
  axis(1,1:ncol(SEQ), labels=colnames(SEQ))
  axis(2,1:nrow(SEQ), labels=rownames(SEQ))
  legend("bottom", c("AA","BB"),lty=1, lwd=3, col=c("red","green"))
  box()
}

