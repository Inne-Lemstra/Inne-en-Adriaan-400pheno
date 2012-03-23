#Samen voegen met van MAnova en rest van de matrix
#(staan veel NA waardes in, dit komt door dat niet alle markers van CombiMatrix ook in Pfac zitten
#en er nog een paar trait zijn waar de 5 basic properties niet in zitten)

prop  <-c(rep(names(Pfac[1]),length(unlist(Pfac[1]))),rep(names(Pfac[2]),length(unlist(Pfac[2]))),rep(names(Pfac[3]),length(unlist(Pfac[3]))),rep(names(Pfac[4]),length(unlist(Pfac[4]))),rep(names(Pfac[5]),length(unlist(Pfac[5]))))
mark  <-c(names(Pfac[[1]]),names(Pfac[[2]]),names(Pfac[[3]]),names(Pfac[[4]]),names(Pfac[[5]]))
value <-as.vector(unlist(Pfac[1:5]))

T3<-cbind(prop,mark,value)
T7<-NULL
for(term in 1:5){
lengthprop<-length(grep(names(Pfac[term]),T3))
n1<-grep(names(Pfac[term]),CombiMatrix[,1])
T6<-NULL
for(x in 1:lengthprop){
T4<-which(T3[x,2]==CombiMatrix[n1,2])
trait<-CombiMatrix[n1,1]
T5<-cbind(as.character(trait[T4]),rep(mark[x],length(trait[T4])),rep(value[x],length(trait[T4])))
T6<-rbind(T6,T5)
}
T7<-rbind(T7,T6)
}
colnames(T7)<-c("Trait","Marker","MAnova")
T8<-voegsamen(CombiMatrix,T7)
