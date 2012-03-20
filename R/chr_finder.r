#(c) Inne
#date created: 7 maart

#Chromosoom/marker finder.
 #Vind de markers op een chromosoom en geeft de Posities in een matrix die deze marker bevatten
 
 per.chr<-function(DATA,Vector,chr){
if(missing(DATA))stop("DATA file is missing")
if(missing(chr)) stop("chromosoom nummer not found")
if(!is.null(dim(Vector))) stop("Lijst met markers moet een Vector zijn")
chrnr<-which(DATA==chr)                                                     #chromosoom nummer bepalen

lijsten<-NULL
for(x in 1:length(chrnr)){
  lijst<- grep(colnames(DATA[,min(chrnr):max(chrnr)])[x],Vector)            
  lijsten<- c(lijsten,lijst)                                                
}
lijsten
}

#DATA= lijst of tabel waaruit de namen van de Markes gehaald moeten worden
#Vector= een vector waarin de markers gevonden moeten worden
#chr= het chromosoom nummer waarvan alle markers bepaalt moeten worden, niet meer dan 1 chromosoom per keer.


