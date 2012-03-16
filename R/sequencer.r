#(c)Inne Lemstra
#date created: 15 maart

#sequence maker

sequencer<- function(Matrix, lijst_voorwaarden,cuttoff)

for(traits in 1:length(lijst_voorwaarden))
  the.rows<-lijst_voorwaarden[trait]==Matrix[,1]
  for(per.row in the.rows)
    if(Matrix[per.row,6]>0 & Matrix[per.row,4]>cuttoff)AABB<-"A"
    if(Matrix[per.row,6]<0 & Matrix[per.row,4]>cuttoff)AABB<-"B"
    if(Matrix[per.row,6]==0 & Matrix[per.row,4]>cuttoff)AABB<-"A/B"
    markAABB<-c(Matrix[per.row,2],AABB)
  if(Matrix[the.row,3]
  
  
  
