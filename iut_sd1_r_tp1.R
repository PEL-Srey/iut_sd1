### EXO-01 ###

data("iris")
ncol(iris)
View(iris)
dim(iris)
iris
class(iris)
colnames(iris)
View(iris).[1] #faux
iris[c(1,5)]
iris[ , c("Sepal.Length","Species")]
iris[c(100,103,105),] #df[c(ligne),c(colonne)] + c() quand on veut spécifier une ligne spécifique ou colonne spécifique
iris[c(50:100),] 
mean(iris[,c("Sepal.Length")])
mean(iris$Sepal.Length)
mean(iris$Petal.Length)
sd(iris$Petal.Length)
median(iris$Sepal.Width)
quantile(iris$Petal.Width,probs = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1))    #correction 
seq(from = 0.1, to = 1.5, by=0.2)



### EXO-02 ###

getwd()
setwd("D:/IUT/Prog R/TP_01")
getwd


dfAnime = read.csv(file ="D:/IUT/Prog R/TP_01/anime.csv", 
         header = TRUE, 
         dec =".",
         sep="," )         # attention c'est / et non \ pour le chemin d'accès !#

dfManga = read.csv(file = "D:/IUT/Prog R/TP_01/manga.csv",
                   header = TRUE,
                   dec = ".",
                   sep = "," )


class(dfAnime)
class(dfManga)

View(dfAnime)
View(dfManga)

dim(dfAnime)
dim(dfManga)

moyAnime = mean(dfAnime$Score)
moyManga = mean(dfManga$Score)
paste("la moyenne pour le manga est de :", moyManga)
paste("la moyenne pour l'animé est de :", moyAnime)

voteAnime = sum(dfAnime$Vote)
voteManga= sum(dfManga$Vote)
paste("le vote pour l'animé est de : ", voteAnime)
paste("le vote pour le manga est de : ", voteManga)


ecartAnime = round(sd(dfAnime$Score)*100,digits = 2)
paste("l'écarte pour l'échantillon l'animé est de : ", ecartAnime,"%")
ecartManga = round(sd(dfManga$Score)*100,digits = 2)
paste("l'écarte pour l'échantillon le manga est de : ", ecartManga,"%")


decAnime = quantile(dfAnime$Score,probs = seq(from = 0.1, to = 0.9, by = 0.1))
paste("Les déciles pour l'animé est de : ", decAnime)
decManga = quantile(dfManga$Score,probs = seq(from=0.1,to=0.9,by=0.1))
print("Les déciles pour le manga est de : ")
print(decManga)




### EXO-03 (a) : Les fonctions subset(), table() et prop.table() ###


nbManga = subset(dfManga, Score > 9, select = "Score")
View(nbManga) # il y en a 10 mangas dont la note est >= 9
nrow(nbManga)

nbManVote = subset(dfManga, Vote > 200000)
nrow(nbManVote) # il y en a 12 lignes


nbManVoSco = sub(x=dfManga, Vote >= 200000 & Score >= 8)
nrow(nbManVoSco)  # ???? Q° prof "Erreur : objet 'Vote' introuvable"

#correction 
extraction3 <- subset(dfManga, Vote >= 200000 & Score >= 8)
nrow(extraction3)


nbMangaSco = subset(dfManga, Score < 8 & Score > 7)  # correction prof borne incluse 
nrow(nbMangaSco)



### EXO-03 (b) : Filtre sur les Animes ###

View(dfAnime)
nrow(dfAnime$Rating)   # Q° prof pouquoi la fonction ne fonctionne pas ici 

efRating = table(dfAnime$Rating)
print(efRating)
length(efRating)
efRatingPor = prop.table(efRating)*100
View(efRating)   # la variabe a 6 modalités indiqué avec la fonction lengh()
View(efRatingPor)

filtre1 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(filtre1)


filtre2=subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score > 8)
nrow(filtre2)


filtre3 = subset(dfAnime,!Rating %in% c("PG - Children","G - All Age"))
nrow(filtre3)


# filter4 = subset(dfAnime, Rating == Score > 9 | Rating == Vote > 400000) => erreur inatention 

filter4 = subset(dfAnime, Score > 9 | Vote > 400000)
nrow(filter4)



### EXO-3 (c) : Les fonctions rbind() et write.table() ###


# DfAM = rbind(dfAnime, dfManga, c("Title","Score","Vote","Ranked")) => erreur compréhension de consigne 

# correction #

dfAnime = dfAnime[,c("Title","Score","Vote","Ranked")]
dfManga = dfManga[,c("Title","Score","Vote","Ranked")]


# dfAnime = dfAnime[, "Type"] => faux 

# correction #

dfAnime$Type = "Anime"
dfManga$Type = "Manga"

dfConcat = rbind(dfAnime,dfManga)
View(dfConcat)

# Extraire = write.table(dfConcat,
                      # file = "ExportTp1.csv",
                     #  sep = ";"             => oublié le ","
                     #  row.names = FALSE) #

# correction # 

getwd()
Extraire = write.table(dfConcat,
                       file = "D:/IUT/Prog R/TP_01/ExportTp1.csv",
                       sep = ",",
                       row.names = FALSE) 










