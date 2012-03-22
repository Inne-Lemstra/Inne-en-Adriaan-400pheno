#(c)Inne Lemstra
#date created: 15 maart

#sequence maker
#was de Bedoeling dat deze functie een sequence AA of BB zou maken, maar Adriaan heeft dit in marker.choice gedaan.
#Werkt (nog) niet
Matrix<-CombiMatrix
lijst_voorwaarden<-c("ABA.NS.AR.Gmax.D.ns.AR.0.5µmABA.Gmax.D.ns","ABA.NS.AR.Gmax.D.ns.AR.0.5µmABA.Gmax.D.ns","ABA.WS.AR.AUC100.D.ws.AR.0.5µmABA.AUC100.D.ws")
lijst_markers<- colnames(chromos)
cuttoff<-NULL

sequencer<- function(Matrix, lijst_voorwaarden,lijst_markers,cuttoff=NULL){
Sequence<-NULL
  for(traits in 1:length(lijst_voorwaarden)){
    the.rows<-lijst_voorwaarden[trait]==Matrix[,1]
    
    for(x in 1:length(the.rows)){
      if(the.rows[x]==FALSE){AABB<-"-"}
      else{
        for(markers in which(the.rows==TRUE)){
        if(!is.null(markers)){AABB<-"-"}
        if(as.numeric(as.character(Matrix[markers,6]))>0){AABB<-"A"}
        if(as.numeric(as.character(Matrix[markers,6]))<0) {AABB<-"B"}
        if(as.numeric(as.character(Matrix[markers,6]))==0){AABB<-"A/B"}
        cat(AABB,"\n")
        }
      }
      Sequence<-c(Sequence,AABB)
    }
    
  }
  Sequence
}
   #if(as.numeric(as.character(Matrix[markers,4]<cuttoff))){AABB<-"-"}
   #& as.numeric(as.character(Matrix[mark,4]>cuttoff))
   #& Matrix[mark,4]>cuttoff
   #& Matrix[mark,4]>cuttoff
   #mark<-which(Matrix[the.rows,2]==lijst_markers[markers])
   #for(per.row in the.rows)
    #if(Matrix[per.row,6]>0 & Matrix[per.row,4]>cuttoff)AABB<-"A"
    #if(Matrix[per.row,6]<0 & Matrix[per.row,4]>cuttoff)AABB<-"B"
    #if(Matrix[per.row,6]==0 & Matrix[per.row,4]>cuttoff)AABB<-"A/B"
    #markAABB<-c(Matrix[per.row,2],AABB)
  #if(Matrix[the.row,3]
  
  #T3<-sequencer(Matrix,lijst_voorwaarden,lijst_markers,cuttoff)
