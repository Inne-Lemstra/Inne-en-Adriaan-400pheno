#(c) Inne
#created: 6 maart

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