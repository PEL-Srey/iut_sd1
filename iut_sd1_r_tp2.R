### EXO-01 ###

getwd()
setwd("E:/IUT/Prog R/TP_02")
getwd()

dfFao=read.csv(file = "E:/IUT/Prog R/TP_02/fao.csv", header = TRUE, sep = ";",dec = ",")

nrow("Nom") # préférer cette fct length(dfFao$Nom)
nrow(dfFao)

summary(dfFao)


### EXO-02 ###


View(dfFao)
moyenneDispoAl = round(mean(dfFao$Dispo_alim),2)

mean(dfFao$Dispo_alim, na.rm=TRUE)  # correction prof : na.rm=TRUE => indiquer d'enlever les valeurs manquants

nbPopMondial= sum(dfFao$Population, na.rm = TRUE)
print(nbPopMondial)


ectProViand = sd(dfFao$Prod_viande, na.rm = TRUE)
ectImpViand = sd(dfFao$Import_viande, na.rm = TRUE) 

medianProViand = median(dfFao$Prod_viande, na.rm = TRUE)
print(medianProViand)

quartilDispAl = quantile(dfFao$Dispo_alim,probs = 0.25, na.rm = TRUE)
print(quartilDispAl)

centilImpViande = quantile(dfFao$Import_viande, prob = 0.75, na.rm = TRUE )
print(centilImpViande)
# correction 
quantile(dfFao$Import_viande, seq(0,1,0.01))  # seq() permet de préciser les bornes qu'on souhaite 


### EXO-03 ###


rankPop = order(dfFao$Population, decreasing = TRUE)
# topDownPays = order(dfFao$Population, decreasing = TRUE)[3,0] = > erreur 
topDownPays = head(dfFao[rankPop,],n=5)
# topDownPays = head(dfFao[order("Population",decreasing = TRUE),], n=5) = > Q° prof 
View(topDownPays)

rankProdV= order(dfFao$Prod_viande,decreasing = TRUE)
topProdV = head(dfFao[rankProdV,],n=5)
View(topProdV)

rankImpV = order(dfFao$Import_viande, decreasing = TRUE)
topImpV = head(dfFao[rankImpV,],n=5)
View(topImpV)

dispoAl = subset(dfFao,Dispo_alim >= 2300 )
length(dispoAl)
nrow(dispoAl)


dispoAlS = subset(dfFao, Dispo_alim > 3500 & Import_viande >= 1000)
View(dispoAlS)
nrow(dispoAlS)

# filtreFrB = subset(dfFao, Nom %int% "France" & "Belgique") => utilisation de c() + pas de & mais "," entre les pays !

filtreFrB = subset(dfFao, Nom %in% c("France","Belgique"))
View(filtreFrB)


### EXO-04 ###


dfFao$part_export = round(dfFao$Export_viande/dfFao$Prod_viande,3)
View(dfFao)

dfFao$dispo_alim_pays = (dfFao$Dispo_alim*dfFao$Population)
View(dfFao)


write.csv(dfFao, file = "ExportTp2.csv ", sep = ",",dec = ".", row.names = FALSE)


sumDispoMon = sum(dfFao$Dispo_alim, na.rm = TRUE)  # pense à ajouter na.rm au calcul pour les valeurs NULL ! 

fleedGlobal = sumDispoMon/2300


### EXO-05 ###


corProdVExV = cor(x=dfFao$Prod_viande,y=dfFao$Export_viande)  #pas nécessaire poru dessiner le nuage de point !
plot(x=dfFao$Prod_viande,y=dfFao$Export_viande,xlab = "Production de viande", ylab = "Exportation de viande", main = "Correlation entre production & exportation de viande")

# corProdVExV = cor(x=dfFao$Prod_viande,y=dfFao$Export_viande, na.rm = TRUE)  => dans le cor pas besoin de na.rm 

corProdVExV = cor(x=dfFao$Prod_viande,y=dfFao$Export_viande)  # Q° prof résultat NA_real ??? 

help("cor")
corGlobal = cor(dfFao[,-1],use = "everything")
View(corGlobal)

#correction prof
matriceCor = cor(dfFao[ , - 1] , use = "complete.obs")    # Q° différent entre everything & cie 
matriceCor = round(matriceCor , 2)
View(matriceCor)

install.packages("corrplot") #pense à metre des "" !

library(corrplot)    #penser à faire appel au librairie ça ne fonctionne pas !
corrplot(matriceCor,method = "pie")












