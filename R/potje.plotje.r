#(c)Inne
#date created:8 maart

#een plotje

#plot grote
plot(c(0,10),c(0,10),type="n")
plot(c(0,69),c(0,10),type="n", xlab="Markers",ylab="Number of LOD above cuttof",axes=FALSE)

#bekijk ?axis
op <- par(las = 2)
op<- par(cex = 0.6)
op <- par(mai=c(2,0.4,0,0))

axis(1,1:ncol(chromos), colnames(chromos))

