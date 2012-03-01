##copyright Adriaan van der Graaf 2012
#doel om data op te halen uit de analysis file.
#daarna opzetten in een matrix:


#aangenomen dat normpeaklist al aanwezig is.
traitmat <- matrix(0, sum(as.numeric(unlist(lapply(normpeaklist,length)))),4)
colnames(traitmat) <- c("Trait","Marker","LOD-value", "Effect AA/BB")

for(i in length(normpeakli