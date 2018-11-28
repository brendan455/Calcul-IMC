#sélection du Dossier de travail
setwd("Directory") 

#importation des données
data<-read.table("exercice2.csv", header=TRUE, sep=";",
                 na.strings="NA", dec=",")

#Calculer la moyenne et l'écart-type de chacune de ces variables
summary(data$age)
sd(data$age)
summary(data$poids)
sd(data$poids)
summary(data$taille)
sd(data$taille)

#Créer les 3 variables (IMC, Obesité, age50plus)
data$imc<-data$poids/(data$taille*data$taille)
data$obesite[data$imc<30]<-0
data$obesite[data$imc>=30]<-1
data$age50plus[data$age>50]<-1
data$age50plus[data$age<=50]<-0

#Créer le tableau de contingence observé des variables "obvesité" et "age50plus"
effectifs<-table(data$age50plus,data$obesite)
tableau<-matrix(c(77,24,8,1), nrow=2, dimnames=list(c("50moins",
                                                      "50plus"), c("normal", "obèse")))

#Afficher le tableau des effectifs théoriques
chisq.test(tableau)$expected

#Les variables "obesite" et "age50plus" sont-elles liées ?
fisher.test(tableau, alternative="two.sided")
