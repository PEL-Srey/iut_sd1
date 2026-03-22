 ### Exercice 1 - Création de fonction ###

## La commande function() ##

# 1 :
salaire_net_cadre = function(salaire_Brute){
  resultat = salaire_Brute*0.75
  print(resultat)
  return(resultat)
}

salaire_net_cadre(salaire_Brute = 2500)

# 2:

salaire_net_cadre = function(salaire_Brute = 2500)     # remarque si on précise une valeur dans le paramètre alors lorsqu'il n'y a pas de valeur celle-ci sera la valeur par défaut !#
  { if(salaire_Brute != 0){
    resultat = salaire_Brute*0.75
    print(resultat)
    return(resultat)}
  else{resultat = 2500*0.75
  print(resultat)
  return(resultat)}
}

salaire_net_cadre()

#correction 
salaire_net_cadre = function(salaire_brut = 2500) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }

                  

# 3 :

salaire_net_cadre = function(salaire_Brut = 2500,temps_Travail = 1) {
  salaire_net_avant_impot = salaire_Brut * 0.75*temps_Travail
  return(salaire_net_avant_impot) }
#Test de la fonction
salaire_net_cadre(4500,0.5)


## La commande if() {} ##

# 4:
salaire_net_cadre = function(salaire_Brut = 2500,temps_Travail = 1) {
  if(is.numeric(salaire_Brut) == FALSE){
    return(print("Erreur"))} else {     # REMARQUE pour que else fonction il faut qu'elle soit sur la même ligne que if !! #
      salaire_net_avant_impot = salaire_Brut * 0.75*temps_Travail
  return(salaire_net_avant_impot) }}

salaire_net_cadre("opo")


#correction 
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}


# 5 :

salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if ((!is.numeric(salaire_brut)) | (temps_travail<=0) | (temps_travail>=1)) {
    return("Erreur :  le salaire brut doit être une valeur numérique et le temps de travail ne doit pas être comprise entre 0 et 1")
  }
  
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}

salaire_net_cadre("opo",1)
salaire_net_cadre("1500",3)

## La commande else() {} ##

# 6 :

salaire_net() = function(statut,salaire_Brut = 2500,temps_Travail = 1) {
  if((is.numeric(salaire_Brut == FALSE))| (temps_Travail<=0) | (temps_Travail >=1) | (statut != "cadre") | (statut != "non cadre")){
    return("Erreur")
  } else { 
  salaire_net_avant_impot = salaire_Brut * 0.75*temps_Travail}
  return(salaire_net_avant_impot) }

# plusieur erreur avec le copiecole : ajout () dv salaire_net + {} mal placé 



#correction 
salaire_net = function(statut,salaire_Brut = 2500,temps_Travail = 1) {
  if(!(is.numeric(salaire_Brut))| (temps_Travail< 0) | (temps_Travail > 1) | !(statut %in% c("cadre","non cadre"))){
    return("Erreur")} else { 
    salaire_net_avant_impot = salaire_Brut * 0.75*temps_Travail
    return(salaire_net_avant_impot) }}


salaire_net("ouvrier",3000,0.8)
salaire_net("cadre",3000,0.8)


## La commande else if() {} #

# 7

salaire_net = function(statut,salaire_Brut = 2500,temps_Travail = 1) {
  if(!(is.numeric(salaire_Brut))| 
     (temps_Travail< 0) | (temps_Travail > 1) | 
     !(statut %in% c("cadre","non cadre"))){
     return("Erreur")} else if (salaire_Brut <= 1591){
      return(c(print("salaire net av impot"),salaire_Brut*0*temps_Travail))} else if(
        salaire_Brut <= 2006){
        return(c(print("salaire net av impot"),salaire_Brut*(1-0.029)*temps_Travail))} else if (
          salaire_Brut <= 3476){ 
          return(c(print("salaire net av impot"),salaire_Brut*(1-0.099)*temps_Travail))} else if (
            salaire_Brut<=8557){ return(c(print("salaire net av impot"),salaire_Brut*(1-0.20)*temps_Travail))} else { 
      salaire_net_avant_impot = salaire_Brut *(1-0.43)*temps_Travail
      return(c(cat(print("salaire net avant impot"),salaire_net_avant_impot,sep =" "))) } }

salaire_net("cadre",8900,1)

salaire_net("cadre",1200,1)

# 8
shifumi = function(choix){
  val_Al = sample(choix1= pierre,choix2=ciseaux,choix3=papier,size = 3,replace = TRUE) if (
    choix == val_Al) { return("Vous avez gagné")} else {return("Perdu !")}}

# correction 
shifumi <- function() {
  # Demander à l'utilisateur de saisir une valeur
  choix_utilisateur <- readline(prompt = "Choisissez entre pierre, papier ou ciseaux : ")
  
  # Vérifier si l'utilisateur a saisi une valeur valide
  if (choix_utilisateur %in% c("pierre", "papier", "ciseaux")) {
    # Simuler un choix aléatoire pour l'ordinateur
    choix_ordi <- sample(c("pierre", "papier", "ciseaux"), 1)
    
    # Afficher les choix de l'utilisateur et de l'ordinateur
    cat("Votre choix :", choix_utilisateur, "\n")
    cat("Choix de l'ordinateur :", choix_ordi, "\n")
    
    # Retourner le résultat du jeu
    if (choix_utilisateur == choix_ordi) {
      return("Égalité !")
    } else if ((choix_utilisateur == "pierre" & choix_ordi == "ciseaux") |
               (choix_utilisateur == "papier" & choix_ordi == "pierre") |
               (choix_utilisateur == "ciseaux" & choix_ordi == "papier")) {
      return("Vous avez gagné !")
    } else {
      return("L'ordinateur a gagné !")
    }
  } else {
    return("Valeur invalide. Veuillez choisir entre pierre, papier ou ciseaux.")
  }
}

#Test de la fonction
shifumi()


### Exercice 2 - Création des boucles ###


# 1

boucle1 = for (i in c(1,2,3,4,5)) { resultat = i+i}
print(boucle1)

#correction 
resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))}

# 2
resultat = 1
while (element <=50) {resultat = resultat + resultat
print(past("le résultat est :", resultat))}  # Q° ,comment arrrêter le programme ?

# correction 
element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", element))
  element = element + 1}

# 3 

#correction 
for (colonne in colnames(iris)) {
  type_colonne = class(iris[ , colonne])
  print(paste("la colonne ", colonne, " est de type : ", type_colonne))
}

# 4
# Initialisation de l'indice de colonne
indice_colonne <- 1

# Tant qu'il reste des colonnes à parcourir dans iris
while (indice_colonne <= ncol(iris)) {
  # Récupération du nom de la colonne
  nom_colonne <- colnames(iris)[indice_colonne]
  
  # Récupération du type de données de la colonne
  type_colonne <- class(iris[, nom_colonne])
  
  # Affichage du résultat
  print(paste("la colonne ", nom_colonne, " est de type : ", type_colonne))
  
  # Passage à la colonne suivante
  indice_colonne <- indice_colonne + 1
}

### Exercice 3 - GOAT : Cas pratiques ###


for ( element in 5) {valeur = readline(prompt = "saisir un nombre :")
      resultat = valeur^2
      print(paste("le resultat est ", resultat))}

#correction :
# Boucle pour demander 5 fois un nombre à l'utilisateur
for (i in 1:5) {
  
  # Demander à l'utilisateur d'entrer un nombre
  nombre <- readline(prompt = "Entrez le nombre :")
  nombre <- as.numeric(nombre)
  
  # Calculer le carré du nombre
  carre <- nombre^2
  
  # Afficher le carré du nombre
  print(paste("Le carré de", nombre, "est", carre))}

# 2
getwd()
setwd("E:/IUT/Algèbre")
getwd()
nb_elment_dos = list.files(path = "E:/IUT/Algèbre")

print(nb_elment_dos) # pas en nb 

for(element in nb_elment_dos){(type_format = file.info("E:/IUT/Algèbre"))
  paste("le type de fichier est :", type_format)}


#correction 
# Chemin du dossier à explorer
dossier <- "chemin/vers/le/dossier"

# Liste les fichiers dans le dossier spécifié
fichiers <- list.files(dossier, full.names = TRUE)

# Affiche la taille de chaque fichier
for (fichier in fichiers) {
  info <- file.info(fichier)
  taille <- info$size
  cat("Le fichier", basename(fichier), "a une taille de", taille, "octets.\n")}

dossier = "E:/IUT/Algèbre"
fichier = list.files(dossier, full.names = TRUE)


for(fichier in fichier){(info = file.info(fichier))
  type_format = info$size
  cat("Le fichier", basename(fichier), "a une taille de", type_format, "octets.\n")}

