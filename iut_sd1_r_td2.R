### EXO-01 ###

getwd()
setwd("E:/IUT/Prog R/TD_02/dataSet")
getwd()

dfbodiesK = read.csv(file="E:/IUT/Prog R/TD_02/dataSet/bodies_karts.csv",
         header = TRUE,
         dec=",",
         sep=";")

dftires = read.csv(file="E:/IUT/Prog R/TD_02/dataSet/tires.csv",
                     header = TRUE,
                     dec=",",
                     sep="") # Pour l'espace il vaut mieux faire \t  [tabulation]#

dfgliders = read.csv(file="E:/IUT/Prog R/TD_02/dataSet/gliders.csv",
                     header = TRUE,
                     dec=".",
                     sep="|")

dfdrivers = read.csv(file="E:/IUT/Prog R/TD_02/dataSet/drivers.csv",
                     header = TRUE,
                     dec=",",
                     sep=";")

dim(dfbodiesK)
dim(dftires)
dim(dfgliders)
dim(dfdrivers)



### EXO-02 ###

summary(dfbodiesK) 
summary(dftires)
summary(dfgliders)
summary(dfdrivers)


plot(x =dfdrivers$Weight,y=dfdrivers$Acceleration,xlab = "Poids", ylab = "Accélération", main = "Corrélation entre le poids et l'accération" ) # rq pas de "" qd on déclare le vecteur !#

coefCorr = cor(x=dfdrivers$Weight,y=dfdrivers$Acceleration, method = "spearman")
print(coefCorr)

r=cov(x=dfdrivers$Weight,y=dfdrivers$Acceleration)/sd(x=dfdrivers$Weight)*sd(y=dfdrivers$Acceleration)

covXY = cov(x=dfdrivers$Weight,y=dfdrivers$Acceleration)
sX = sd(dfdrivers$Weight)
sY = sd(dfdrivers$Acceleration)
print(covXY/(sX*sY))

coefDet = coefCorr^2
print(coefDet)

matriceCor = round(cor(dfdrivers[,-1]),2)
View(matriceCor)

install.packages("corrplot")   # import coorplot #
library(corrplot)


corrplot(matriceCor, type = "upper", order = "hclust",col = c("green","yellow","blue"))
corrplot(matriceCor, method = "pie")


matriceCorBodiesk = round(cor(dfbodiesK[ ,-1]),2)
corrplot(matriceCorBodiesk, method = "circle")

matriceCorGliders = round(cor(dfgliders[,-1]),2)
corrplot(matriceCorGliders, type = "lower" )


matriceCorTitres = round(cor(dftires[,-1]),2)
col = colorRampPalette(c("black","purple","pink"))(20)
corrplot(matriceCorTitres, method = "number", col = col)




### EXO-03  ###

resultat = dfdrivers(Driver=c(Weight)) # faux ! #
resultat = dfdrivers[,c("Driver", "Weight")]
View(resultat)

resultat2=dfdrivers[1:10,c("Driver","Acceleration")]
View(resultat2)

resultat3 = dfdrivers[,c(1:4,6,8,10:14)]
View(resultat3)
resultat3Correct = dfdrivers[,-c(5,7,9)]
View(resultat3Correct)

resultat4=dfdrivers[,-c("Weight","Acceleration")]     #- dv c ne fonctionne que sur des opérateurs NUMERIQUE ! #
resultat4bon= dfdrivers[,c(2,3)]
View(resultat4bon)

resultat5=dfdrivers[,c("Driver","Acceleration","Weight")]
View(resultat5)

resultat6= dfdrivers[c(3,12,32),1]
View(resultat6)
resultat6Bis= dfdrivers[c(3,12,32),"Driver"]
View(resultat6Bis)
resultat6Bon = dfdrivers[c(3,12,32),]   # ??? tous les colonnes s'affichent incohérent avec la consigne // Q° prof ?#
View(resultat6Bon)

resultat7= dfdrivers[c(32,3,12),]   # rien d'anormal ?! hormis que tous les colonnes s'affichent #
View(resultat7)

# # resultat8=dfdrivers[, c("Driver",(order("Weight"),decreasing =FALSE))]  =>  Mauvais code #
resultat8 =dfdrivers[, c("Driver","Weight")
## View(order(resultat8[,"Weight"],decreasing =FALSE)) => mauvais code ##
## View(order("Weight",decreasing =FALSE)) => non plus ##
## resultat8Ordo = order(resultat8[,"Weight"],decreasing =FALSE) => idem ##
View(resultat8Ordo)

rang = order(dfdrivers$Weight)
resultat8Bon = dfdrivers[rang,c("Driver","Weight")]  # RQ d'abord la manip ranger puis création d'objet !
View(resultat8Bon)

rang = order(dfdrivers$Acceleration, decreasing = TRUE)
resultat9 = dfdrivers[rang,c("Driver","Acceleration")]
View(resultat9)

rangVite= order(dfdrivers$Acceleration,decreasing = TRUE)
rangPoids = order(dfdrivers$Weight, decreasing = FALSE)
## resultat10 = dfdrivers[c(rangPoids,rangVite),c("Driver","Weight","Acceleration")] => le trie n'a pas fonctionné ?!
resultat10_1= dfdrivers[rangPoids,c("Driver","Weight","Acceleration")]
View(resultat10_1)
resultat10_2 = dfdrivers[rangVite,c("Driver","Weight","Acceleration")]
View(resultat10_2)

rangBon = order(dfdrivers$Acceleration, dfdrivers$Weight, decreasing = c(TRUE,FALSE))
resultat10Bon = dfdrivers[rangBon,c("Driver","Weight","Acceleration")]
View(resultat10Bon)  # Q° au prof ?




### EXO-04 ###


# topDriver = dfdrivers[rangDriv = order(dfdrivers$Acceleration,decreasing = FALSE,c("Driver","Acceleration"))] => Q° prof sur l'argument imbriqué #
rangDriv = order(dfdrivers$Acceleration,decreasing = FALSE)
topDriver = dfdrivers[rangDriv,c("Driver","Acceleration")] 
View((topDriver))

#correction prof 
help("subset") # accès doc depuis IDE

topDriverBon = subset(x <-dfdrivers,
                      subset = Acceleration == max(Acceleration),
                      select = c("Driver","Acceleration"))
View(topDriverBon)

topDriverTest = subset(x=dfdrivers,
                       subset = Acceleration == order(Acceleration, decreasing = FALSE),
                       select = c("Driver","Acceleration"))   # Q° sur le nb d'affiche des lignes 
View(topDriverTest)

topBodiesk = subset(x=dfbodiesK, 
                    subset = Acceleration == max(Acceleration),
                    select = c("Body","Acceleration"))
View(topBodiesk)

topGliders = subset(x=dfgliders,
                    subset = Acceleration == max(Acceleration),
                    select = c("Glider","Acceleration"))
View(topGliders)

topTires = subset(x=dftires,
                  subset = Acceleration == max(Acceleration),
                  select = c("Tire","Acceleration"))
View(topTires)
