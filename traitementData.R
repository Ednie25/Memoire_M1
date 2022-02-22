
#Appel des librairies que nous utiliserons

library(ggplot2)
library(tseries)
library(aTSA)
library(ARDL)
library(dynamac)
library('dLagM')
        
#Evolution de la rentabilite

serie1<-data.frame(x=database1$Periode,y=database1$Rentabilite) #Variables que nous voulons representer 
p1 <- ggplot(serie1, aes(x=database1$Periode,y=database1$Rentabilite)) + geom_line() + ggtitle("Evolution de la rentabilite") + 
  xlab("") + ylab("value") #Graphique 
p1 #Representation 


#Evolution de l'investissement 

serie2<-data.frame(x=database1$Periode,y=database1$Investissement) #Variables que nous voulons representer
p2 <- ggplot(serie2, aes(x=database1$Periode,y=database1$Investissement)) + geom_line() + ggtitle("Evolution de l'investissement") + 
  xlab("") + ylab("value") #Graphique 
p2 #Representation 


#Evolution des concours publics

serie3<-data.frame(x=database1$Periode,y=database1$`CONCOURS PUBLICS`)
p3 <- ggplot(serie3, aes(x=database1$Periode,y=database1$`CONCOURS PUBLICS`)) + geom_line() + ggtitle("Evolution des concours publics") + 
  xlab("") + ylab("value")
p3

serie4<-data.frame(x=database1$Periode,y=database1$`offre commerciale`)
p4 <- ggplot(serie4, aes(x=database1$Periode,y=database1$`offre commerciale`)) + geom_line() + ggtitle("Evolution de l'offre commerciale") + 
  xlab("") + ylab("value")
p4

#Matrice correlation entre les variables
matrice<-data.frame(database1$Rentabilite,database1$Investissement,database1$`CONCOURS PUBLICS`,database1$`offre commerciale` )
mcor <- cor(matrice)
mcor


#Relation entre les variables
  #Rentabilite et investissement

relation1<-data.frame(x= database1$Investissement,y=database1$Rentabilite)
g1<-ggplot(relation1, aes(x,y)) + geom_point() + ggtitle("Relation entre la rentabilité et les investissements en environnement") + 
  xlab("Investissement") + ylab("Rentabilité")
g1


  #Rentabilite et concours publics
relation2<-data.frame(x= database1$`CONCOURS PUBLICS`,y=database1$Rentabilite)
g2<-ggplot(relation2, aes(x,y)) + geom_point() + ggtitle("Relation entre la rentabilité et les concours publics") + 
  xlab("Concours Publics") + ylab("Rentabilité")
g2



relation3<-data.frame(x= database1$`offre commerciale`,y=database1$Rentabilite)
g3<-ggplot(relation3, aes(x,y)) + geom_point() + ggtitle("Relation entre la rentabilité et l'offre commerciale") + 
  xlab("offre commerciale") + ylab("Rentabilité")
g3


