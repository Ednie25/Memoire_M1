

#mensualisation de la rentabilité

rentabilite_mensuelle <- ts(database1$Rentabilite,start=2000) #Transformation en série temporelle
rentabilite_mensuelle <- predict(td(rentabilite_mensuelle~ 1,to="monthly",method = "denton-cholette", conversion = "average")) #désagregation de la série
rentabilite=write.csv(rentabilite_mensuelle,"rentabilite.csv") #Ecriture du fichier en csv

#mensualisation de l'investissement 

investissement<-ts(database1$Investissement,start=2000)
investissement_mensuel <- predict(td(investissement~ 1,to="monthly",method = "denton-cholette", conversion = "average"))
investissement_mensuel=write.csv(investissement_mensuel,"investissement.csv")

#mensualisation des concours publics 

concoursPublics<-ts(database1$`CONCOURS PUBLICS`,start=2000)
concoursPublicsMensuel<-predict(td(concoursPublics~ 1,to="monthly",method = "denton-cholette", conversion = "average"))
concoursPublicsMensuel=write.csv(concoursPublicsMensuel,"concoursPublic.csv")

#mensualisation de l'offre commercial 

offreCommercial<-ts(database1$`offre commerciale`,start=2000)
offreCommercialMensuel<-predict(td(offreCommercial~ 1,to="monthly",method = "denton-cholette", conversion = "average"))
offreCommercialsMensuel=write.csv(offreCommercialMensuel,"offreCommercial.csv")

#Analyse descriptive des variables

library(ggplot2)

#Evolution de la rentabilite

serie1<-data.frame(x=databaseFinale$mois,y=databaseFinale$rentabilite)
p1 <- ggplot(serie1, aes(x=databaseFinale$mois,y=databaseFinale$rentabilite)) + geom_line() + ggtitle("Evolution de la rentabilite") + 
  xlab("") + ylab("value")
p1

#Evolution de l'investissement 

serie2<-data.frame(x=databaseFinale$mois,y=databaseFinale$investissement)
p2 <- ggplot(serie1, aes(x=databaseFinale$mois,y=databaseFinale$investissement)) + geom_line() + ggtitle("Evolution de l'investissement") + 
  xlab("") + ylab("value")
p2

#Evolution des concours publics

serie3<-data.frame(x=databaseFinale$mois,y=databaseFinale$concoursPublilc)
p3 <- ggplot(serie1, aes(x=databaseFinale$mois,y=databaseFinale$concoursPublilc)) + geom_line() + ggtitle("Evolution des concours publics") + 
  xlab("") + ylab("value")
p3

#Relation avec la variable dependate: Nuage de points 

relation1<-data.frame(x= databaseFinale$investissement,y=databaseFinale$rentabilite)
g1<-ggplot(relation1, aes(x,y)) + geom_point() + ggtitle("Relation entre la rentabilité et les investissement en environnement") + 
  xlab("Investissement") + ylab("Rentabilité")
g1

relation2<-data.frame(x= databaseFinale$concoursPublilc,y=databaseFinale$rentabilite)
g2<-ggplot(relation1, aes(x,y)) + geom_point() + ggtitle("Relation entre la rentabilité et les investissement en environnement") + 
  xlab("Investissement") + ylab("Rentabilité")
g2

#matrice de correlation entre les variables
matrice<-data.frame(databaseFinale$rentabilite,databaseFinale$investissement,databaseFinale$concoursPublilc, databaseFinale$offrecommerciale)
mcor <- cor(matrice)
mcor


library(tseries)
library(aTSA)


#Stationnarité pour la rentabilité

stationary.test(databaseFinale$Rentabilite) 
stationary.test(databaseFinale$Rentabilite, method = "pp")
stationary.test(databaseFinale$Rentabilite, method = "kpss")


#Stationnarité pour l'investissement 

stationary.test(databaseFinale$Investissement) 
stationary.test(databaseFinale$Investissement, method = "pp")
stationary.test(databaseFinale$Investissement, method = "kpss")

#En difference premiere

stationary.test(databaseFinale$deltaInvestissement) 
stationary.test(databaseFinale$deltaInvestissement, method = "pp")
stationary.test(databaseFinale$deltaInvestissement, method = "kpss")



#Stationnarité pour les concours publics 

stationary.test(databaseFinale$concoursPublics) 
stationary.test(databaseFinale$concoursPublics, method = "pp")
stationary.test(databaseFinale$concoursPublics, method = "kpss")

#En difference premiere

stationary.test(databaseFinale$deltaConcoursPublics) 
stationary.test(databaseFinale$deltaConcoursPublics, method = "pp")
stationary.test(databaseFinale$deltaConcoursPublics, method = "kpss")

stationary.test(databaseFinale$offrecommerciale) 
stationary.test(databaseFinale$offrecommerciale, method = "pp")
stationary.test(databaseFinale$offrecommerciale, method = "kpss")

#En difference premiere

stationary.test(databaseFinale$deltaOffrecommerciale) 
stationary.test(databaseFinale$deltaOffrecommerciale, method = "pp")
stationary.test(databaseFinale$deltaOffrecommerciale, method = "kpss")



data2<- data.frame(x=databaseFinale$Rentabilite, y=databaseFinale$deltaInvestissement, z=databaseFinale$deltaConcoursPublics, w=databaseFinale$deltaOffrecommerciale) #Variables qui figureront dans la modélisation

VARselect(databaseFinale$Rentabilite)
VARselect(databaseFinale$deltaInvestissement)
VARselect(databaseFinale$deltaConcoursPublics)
VARselect(databaseFinale$deltaOffrecommerciale)
res1 <- dynardl(x ~ y + z+w, data = data2,
                lags = list("x" = 3, "y"=2, "z"=2, "w"=4),noLDV = TRUE)
summary(res1)

install.packages('dLagM') #Package pour ARDL

remove <- list(p = list(y = c(3,4), z=c(3,4)))
models2=ardlDlm(x ~ y+z +w,data = data2,p=4, q=3, remove = remove)
summary(models2)
ardlBoundOrders(data = data2, x ~ y+z +w, max.p = 4, max.q = 3)
ardlBound(data = data2, x ~ y+z+w, case = 1, max.p = 1, max.q = 2 )



#Test de causalité

data<-data.frame(databaseFinale$Rentabilite,databaseFinale$deltaInvestissement)
R<-as.numeric(databaseFinale$Rentabilite) 
I<-as.numeric(databaseFinale$deltaInvestissement)
CP<-as.numeric(databaseFinale$deltaConcoursPublics)
OC<-as.numeric(databaseFinale$deltaOffrecommerciale)


#Analyse de causalité (Rentabilité sur les autres)

grangertest(R,I, order=2)
grangertest(R,CP, order=2)
grangertest(R,OC, order=2)

#Analyse de causalité (Investissement sur les autres)

grangertest(I,R, order=2)
grangertest(I,CP, order=2)
grangertest(I,OC, order=2)

#Analyse de causalité (Concours publics sur les autres)

grangertest(CP,R, order=2)
grangertest(CP,I, order=2)
grangertest(CP,OC, order=2)

#Analyse de causalité (Offre Commerciale sur les autres)

grangertest(OC,R, order=2)
grangertest(OC,I, order=2)
grangertest(OC,CP, order=2)
