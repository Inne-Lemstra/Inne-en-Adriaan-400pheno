#(c) Inne
#created: 6 maart

#De samenvoeg functie
voegsamen<-function(Matrix1,Matrix2){
  merge(Matrix1,Matrix2, all.x=TRUE,all.y=TRUE)
}

#T-test Matrix
traitmat <- trait.marker.list(peak.mat.row(tmat,3,phenotypes))

#Anova's
Anovamat <-anova.mat(phenotypes,genotypes)

lijstAno<-peak.mat.row(Anovamat,3,phenotypes)
Anomatrix<-trait.marker.list(lijstAno)

#Welke traits zijn dubbel en welke niet
comp<-traitmat[,1]%in%Anomatrix[,1]
rcomp<- Anomatrix[,1]%in%traitmat[,1]

UniekT <-which(comp==FALSE)
UniekA <- which(rcomp==FALSE)
#Unieken uit matrixes halen
T1<-(1:nrow(traitmat))%in%UniekT
T2<-which(T1==FALSE)
UniT <- traitmat[T2,]

T3<-(1:nrow(Anomatrix))%in%UniekA
T4<-which(T3==FALSE)
UniA <- Anomatrix[T4,]


