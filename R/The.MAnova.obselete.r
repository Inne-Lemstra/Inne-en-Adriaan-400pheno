#Samen voegen met van MAnova en rest van de matrix
#(staan veel NA waardes in, dit komt door dat niet alle markers van CombiMatrix ook in Pfac zitten
#en er nog een paar trait zijn waar de 5 basic properties niet in zitten)


#het bij elkaar halen van properties, markers en P-value's
prop  <-c(rep(names(Pfac[1]),length(unlist(Pfac[1]))),rep(names(Pfac[2]),length(unlist(Pfac[2]))),rep(names(Pfac[3]),length(unlist(Pfac[3]))),rep(names(Pfac[4]),length(unlist(Pfac[4]))),rep(names(Pfac[5]),length(unlist(Pfac[5]))))
mark  <-c(names(Pfac[[1]]),names(Pfac[[2]]),names(Pfac[[3]]),names(Pfac[[4]]),names(Pfac[[5]]))
value <-as.vector(unlist(Pfac[1:5]))

T3<-cbind(prop,mark,value)      #De bovengenoemde waarden aan elkaar binden 
T7<-NULL
for(term in 1:5){                                   #gaat alle lijsten af om de namen van pfac in de samen te voegen matric te vinden
lengthprop<-length(grep(names(Pfac[term]),T3))      
n1<-grep(names(Pfac[term]),CombiMatrix[,1])
T6<-NULL
for(x in 1:lengthprop){                            #het verdubellen vande markers (inclusief values en properties) zodat hij te meregn is met een andere matrix
T4<-which(T3[x,2]==CombiMatrix[n1,2])              #opzoeken welke markergelijk is
trait<-CombiMatrix[n1,1]                           #gelijk namige trait halen uit pa
T5<-cbind(as.character(trait[T4]),rep(mark[x],length(trait[T4])),rep(value[x],length(trait[T4])))  #aan elkaar binden traits met markers een aantal keer herhaalt
T6<-rbind(T6,T5)
}
T7<-rbind(T7,T6)
}
colnames(T7)<-c("Trait","Marker","MAnova")         #kollom namen goed zetten
T8<-voegsamen(CombiMatrix,T7)                      #samen voegen met eerder gemaakte matrix (met T.test en Anova)
