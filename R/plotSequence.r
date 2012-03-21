#(c)Inne
# Date created: 21-03-2012

# een functie om een matrix met sequences (AAof BB) in een image te plotten

plotSequence<-function(uitkomst_marker.choice){
SEQ<-NULL
SEQ<-uitkomst_marker.choice

SEQ[which(SEQ=="AA")]<-1
SEQ[which(SEQ=="BB")]<-2
SEQ[which(SEQ=="-")]<-3

op<-par(las=2)
op<-par(cex=0.4)
op<-par(mai=c(2,0.5,0,0))
image(1:ncol(SEQ),1:nrow(SEQ), t(apply(SEQ,2,as.numeric)),col=c("green","red","white"),xlab="Traits", ylab="Markers",xaxt="n",yaxt="n")
axis(1,1:ncol(SEQ),1:nrow(SEQ), labels=colnames(SEQ))
axis(2,1:nrow(SEQ), labels=rownames(SEQ))
legend("bottom", c("AA","BB"),lty=1, lwd=3, col=c("red","green"))
box()
}

