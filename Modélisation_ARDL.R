

library(ARDL)
library(dynamac)
library('dLagM')


#stationnarite des series

#Rentabilite (stationnaire en niveau  sans tendance ni constante)
stationary.test(database1$Rentabilite) #Dickey-Fuller Augmented 
stationary.test(database1$Rentabilite, method = "pp") # Phillipe-Perron
stationary.test(database1$Rentabilite, method = "kpss") 


#Investissement (stationnaire en difference premiere avec tendance et constante)
stationary.test(database1$Investissement) 
stationary.test(database1$Investissement, method = "pp")
stationary.test(database1$Investissement, method = "kpss")
#Nous prenons la variable en difference premiere
stationary.test(database1$deltaInvestissement) 
stationary.test(database1$deltaInvestissement, method = "pp")
stationary.test(database1$deltaInvestissement, method = "kpss")


#Concours Publics (stationnaire en difference premiere avec constante sans tendance)
stationary.test(database1$`CONCOURS PUBLICS`) 
stationary.test(database1$`CONCOURS PUBLICS`, method = "pp")
stationary.test(database1$`CONCOURS PUBLICS`, method = "kpss")
#Difference premiere
stationary.test(database1$`deltaCONCOURS PUBLICS`) 
stationary.test(database1$`deltaCONCOURS PUBLICS`, method = "pp")
stationary.test(database1$`deltaCONCOURS PUBLICS`, method = "kpss")


stationary.test(database1$`offre commerciale`) 
stationary.test(database1$`offre commerciale`,method = "pp")
stationary.test(database1$`offre commerciale`, method = "kpss")

stationary.test(database1$`delta offre commerciale`) 
stationary.test(database1$`delta offre commerciale`,method = "pp")
stationary.test(database1$`delta offre commerciale`, method = "kpss")



#Estimation ardl 


data<-data.frame(x=database1$Rentabilite,y=database1$InvestissementFinal, z=database1$`deltaCONCOURS PUBLICS`, w=database1$`delta offre commerciale`)

modeleChoix<-auto_ardl(x~y+z+w, data=data,max_order = c(3,3,3,3))
modeleChoix$best_order
summary(modeleChoix$best_model) # ordre retenu pour adrl est ARDL(1,0,0,0) 
data2<-data.frame(x=database1$Rentabilite,y=database1$InvestissementFinal, z=database1$`CONCOURS PUBLICS`, w=database1$`delta offre commerciale`)
res1 <- dynardl(x ~ y + z+w, data = data2,
                lags = list("x" = 1, "y"=3, "z"=3, "w"=3))

summary(res1)


#Test de co integration 
#Estimation du modele a correction d'erreur 
res4 <- dynardl(x ~ y + z+w, data = data2,
                lags = list("x" = 1, "y"=1, "z"=1, "w"=1), diffs = c("y","z"), ec=TRUE)
summary(res4)

dynardl.auto.correlated(res4)
pssbounds(res4)
models2=ardlDlm(x ~ y +z+w,data = data2, p=3, q=3 )
summary(models2)





