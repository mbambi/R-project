#étape 1 : Definition des donnees 

#Objet de l'etude : Etude statistique sur la perception
# des etudiants sur l'adoption de l'IA

#Problematique : Les etudiants ont une negative perception 
#sur l'adoption de l'IA 

#Population ciblee :les etudiants de ENSAK

#Question de recherche : Quelle est la perception des etudiants
#D'ENSAK sur l'adoption de l'IA?

# Il s'agit d'une etude explorative qualitative



#-------------------Hypotheses de recherche :------------------------

#H1 : Le niveau de familiarité avec les outils numériques influence la perception des avantages de l'IA dans l'amélioration de l'expérience éducative.

#H2 : La personnalisation des contenus d’apprentissage est perçue comme le principal avantage de l’IA pour enrichir l’expérience éducative dans l’enseignement supérieur.

#H3 : L’optimisation des programmes éducatifs grâce à l’analyse des données influence positivement les méthodes d’apprentissage traditionnelles.

#H4 : Les préoccupations éthiques liées à la confidentialité des données constituent la principale inquiétude limitant l’adoption de l’IA dans l’enseignement supérieur.

#H5 : Les étudiants préfèrent un rôle de soutien administratif et complémentaire pour l’IA, ce qui réduit la crainte d’un remplacement des enseignants.

#H6 : La transparence sur les algorithmes et la formation des utilisateurs sont des conditions essentielles pour une adoption réussie de l’IA dans l’enseignement supérieur.

#-------------------------------------------------------------




#-------------------------------------------------------------
# étape 2 : Collecte de données
#-------------------------------------------------------------

library(readxl)
data <- read_excel("data/Questionnaire _ La perception des étudiants sur l'adoption de l'IA dans l'enseignement supérieur  (réponses).xlsx")
View(data)

#---------------- Démarche : -------------------------------------

# Pour savoir le type primitif de chaque variable dans notre dataframe, 
# on teste dans la console avec la commande class(data$nom_var).

# Les variables de type caractère sont codifiées, leurs modalités sont placées à gauche.
# => On codifie les variables de type caractère en les convertissant en factor.

# Puisque dans notre formulaire on s'est basé sur l'échelle de Likert, on a 
# besoin de faire un test de fiabilité, et donc on doit convertir les variables en numeric.

# À cette étape, on va codifier les modalités de l'échelle de Likert.

#-------------------------------------------------------------

# étape 3 : Pré-traitement
#-------------------------------------------------------------

# 3.1 Conversion des variables

if (is.character(data$Genre)) {
  data$Genre = as.factor(data$Genre)
}
if (is.character(data$Filiere)) {
  data$Filiere = as.factor(data$Filiere)
}
if (is.character(data$Niveau)) {
  data$Niveau = as.factor(data$Niveau)
}
if (is.character(data$familiariteAI)) {
  data$familiariteAI = as.factor(data$familiariteAI)
}
if (is.character(data$experienceAI)) {
  data$experienceAI = as.factor(data$experienceAI)
}

#-------------------------------------------------------------

#Conversions pour les graphes

#-------------------------------------------------------------

# Conversion de Q1
if (is.character(data$Q1)) {
  data$Q1 = factor(data$Q1)
}

# Conversion des items Q2
if (is.character(data$Q2item1)) {
  data$Q2item1 = factor(data$Q2item1)
}
if (is.character(data$Q2item2)) {
  data$Q2item2 = factor(data$Q2item2)
}
if (is.character(data$Q2item3)) {
  data$Q2item3 = factor(data$Q2item3)
}
if (is.character(data$Q2item4)) {
  data$Q2item4 = factor(data$Q2item4)
}

# Conversion des items Q3
if (is.character(data$Q3item1)) {
  data$Q3item1 = factor(data$Q3item1)
}
if (is.character(data$Q3item2)) {
  data$Q3item2 = factor(data$Q3item2)
}
if (is.character(data$Q3item3)) {
  data$Q3item3 = factor(data$Q3item3)
}
if (is.character(data$Q3item4)) {
  data$Q3item4 = factor(data$Q3item4)
}

# Conversion des items Q4
if (is.character(data$Q4item1)) {
  data$Q4item1 = factor(data$Q4item1)
}
if (is.character(data$Q4item2)) {
  data$Q4item2 = factor(data$Q4item2)
}
if (is.character(data$Q4item3)) {
  data$Q4item3 = factor(data$Q4item3)
}
if (is.character(data$Q4item4)) {
  data$Q4item4 = factor(data$Q4item4)
}

# Conversion des items Q5
if (is.character(data$Q5item1)) {
  data$Q5item1 = factor(data$Q5item1)
}
if (is.character(data$Q5item2)) {
  data$Q5item2 = factor(data$Q5item2)
}
if (is.character(data$Q5item3)) {
  data$Q5item3 = factor(data$Q5item3)
}

# Conversion des items Q6
if (is.character(data$Q6item1)) {
  data$Q6item1 = factor(data$Q6item1)
}
if (is.character(data$Q6item2)) {
  data$Q6item2 = factor(data$Q6item2)
}
if (is.character(data$Q6item3)) {
  data$Q6item3 = factor(data$Q6item3)
}
if (is.character(data$Q6item4)) {
  data$Q6item4 = factor(data$Q6item4)
}

#-------------------------------------------------------------

#The graphs:

hist(data$age)
plot(data$Genre)
plot(data$Niveau)
plot(data$Filiere)
plot(data$Q1)
plot(data$Q2item1)
plot(data$Q2item2)
plot(data$Q2item3)
plot(data$Q2item4)
plot(data$Q3item1)
plot(data$Q3item2)
plot(data$Q3item3)
plot(data$Q3item4)
plot(data$Q4item1)
plot(data$Q4item2)
plot(data$Q4item3)
plot(data$Q4item4)
plot(data$Q5item1)
plot(data$Q5item2)
plot(data$Q5item3)
plot(data$Q6item1)
plot(data$Q6item2)
plot(data$Q6item3)
plot(data$Q6item4)

#-------------------------------------------------------------


#Codification des donnees : 5: Tout a fait d'accord --> 1 :Pas du tout d'accord

data[["Q1"]] <- ifelse(data[["Q1"]] == "1 (Pas du tout)", 1,
                       ifelse(data[["Q1"]] == "2 (Peu)", 2,
                              ifelse(data[["Q1"]] == "3 (Moyennement)", 3,
                                     ifelse(data[["Q1"]] == "4 (Beaucoup)", 4,
                                            ifelse(data[["Q1"]] == "5 (Énormément)", 5, data[["Q1"]])))))

data[["Q2item1"]] <- ifelse(data[["Q2item1"]] == "Pas du tout d’accord", 1,
                       ifelse(data[["Q2item1"]] == "Pas d'accord", 2,
                              ifelse(data[["Q2item1"]] == "Neutre", 3,
                                     ifelse(data[["Q2item1"]] == "D'accord", 4,
                                            ifelse(data[["Q2item1"]] == "Tout a fait d'accord", 5, data[["Q2item1"]])))))

data[["Q2item2"]] <- ifelse(data[["Q2item2"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q2item2"]] == "Pas d'accord", 2,
                                   ifelse(data[["Q2item2"]] == "Neutre", 3,
                                          ifelse(data[["Q2item2"]] == "D'accord", 4,
                                                 ifelse(data[["Q2item2"]] == "Tout a fait d'accord", 5, data[["Q2item2"]])))))

data[["Q2item3"]] <- ifelse(data[["Q2item3"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q2item3"]] == "Pas d'accord", 2,
                                   ifelse(data[["Q2item3"]] == "Neutre", 3,
                                          ifelse(data[["Q2item3"]] == "D'accord", 4,
                                                 ifelse(data[["Q2item3"]] == "Tout a fait d'accord", 5, data[["Q2item3"]])))))

data[["Q2item4"]] <- ifelse(data[["Q2item4"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q2item4"]] == "Pas d'accord", 2,
                                   ifelse(data[["Q2item4"]] == "Neutre", 3,
                                          ifelse(data[["Q2item4"]] == "D'accord", 4,
                                                 ifelse(data[["Q2item4"]] == "Tout a fait d'accord", 5, data[["Q2item4"]])))))

data[["Q3item1"]] <- ifelse(data[["Q3item1"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q3item1"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q3item1"]] == "Neutre", 3,
                                          ifelse(data[["Q3item1"]] == "D'accord", 4,
                                                 ifelse(data[["Q3item1"]] == "Tout à fait d'accord", 5, data[["Q3item1"]])))))

data[["Q3item2"]] <- ifelse(data[["Q3item2"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q3item2"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q3item2"]] == "Neutre", 3,
                                          ifelse(data[["Q3item2"]] == "D'accord", 4,
                                                 ifelse(data[["Q3item2"]] == "Tout à fait d'accord", 5, data[["Q3item2"]])))))

data[["Q3item3"]] <- ifelse(data[["Q3item3"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q3item3"]] == "Pas d'accord", 2,
                                   ifelse(data[["Q3item3"]] == "Neutre", 3,
                                          ifelse(data[["Q3item3"]] == "D'accord", 4,
                                                 ifelse(data[["Q3item3"]] == "Tout à fait d'accord", 5, data[["Q3item3"]])))))

data[["Q3item4"]] <- ifelse(data[["Q3item4"]] == "Pas du tout d'accord", 1,
                            ifelse(data[["Q3item4"]] == "Pas d'accord", 2,
                                   ifelse(data[["Q3item4"]] == "Neutre", 3,
                                          ifelse(data[["Q3item4"]] == "D'accord", 4,
                                                 ifelse(data[["Q3item4"]] == "Tout à fait d'accord", 5, data[["Q3item4"]])))))

data[["Q4item1"]] <- ifelse(data[["Q4item1"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q4item1"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q4item1"]] == "Neutre", 3,
                                          ifelse(data[["Q4item1"]] == "D’accord", 4,
                                                 ifelse(data[["Q4item1"]] == "Tout à fait d’accord", 5, data[["Q4item1"]])))))

data[["Q4item2"]] <- ifelse(data[["Q4item2"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q4item2"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q4item2"]] == "Neutre", 3,
                                          ifelse(data[["Q4item2"]] == "D’accord", 4,
                                                 ifelse(data[["Q4item2"]] == "Tout à fait d’accord", 5, data[["Q4item2"]])))))

data[["Q4item3"]] <- ifelse(data[["Q4item3"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q4item3"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q4item3"]] == "Neutre", 3,
                                          ifelse(data[["Q4item3"]] == "D’accord", 4,
                                                 ifelse(data[["Q4item3"]] == "Tout à fait d’accord", 5, data[["Q4item3"]])))))

data[["Q4item4"]] <- ifelse(data[["Q4item4"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q4item4"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q4item4"]] == "Neutre", 3,
                                          ifelse(data[["Q4item4"]] == "D’accord", 4,
                                                 ifelse(data[["Q4item4"]] == "Tout à fait d’accord", 5, data[["Q4item4"]])))))

data[["Q5item1"]] <- ifelse(data[["Q5item1"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q5item1"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q5item1"]] == "Neutre", 3,
                                          ifelse(data[["Q5item1"]] == "D’accord", 4,
                                                 ifelse(data[["Q5item1"]] == "Tout à fait d’accord", 5, data[["Q5item1"]])))))

data[["Q5item2"]] <- ifelse(data[["Q5item2"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q5item2"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q5item2"]] == "Neutre", 3,
                                          ifelse(data[["Q5item2"]] == "D’accord", 4,
                                                 ifelse(data[["Q5item2"]] == "Tout à fait d’accord", 5, data[["Q5item2"]])))))

data[["Q5item3"]] <- ifelse(data[["Q5item3"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q5item3"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q5item3"]] == "Neutre", 3,
                                          ifelse(data[["Q5item3"]] == "D’accord", 4,
                                                 ifelse(data[["Q5item3"]] == "Tout à fait d’accord", 5, data[["Q5item3"]])))))

data[["Q6item1"]] <- ifelse(data[["Q6item1"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q6item1"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q6item1"]] == "Neutre", 3,
                                          ifelse(data[["Q6item1"]] == "D’accord", 4,
                                                 ifelse(data[["Q6item1"]] == "Tout à fait d’accord", 5, data[["Q6item1"]])))))

data[["Q6item2"]] <- ifelse(data[["Q6item2"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q6item2"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q6item2"]] == "Neutre", 3,
                                          ifelse(data[["Q6item2"]] == "D’accord", 4,
                                                 ifelse(data[["Q6item2"]] == "Tout à fait d’accord", 5, data[["Q6item2"]])))))

data[["Q6item3"]] <- ifelse(data[["Q6item3"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q6item3"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q6item3"]] == "Neutre", 3,
                                          ifelse(data[["Q6item3"]] == "D’accord", 4,
                                                 ifelse(data[["Q6item3"]] == "Tout à fait d’accord", 5, data[["Q6item3"]])))))

data[["Q6item4"]] <- ifelse(data[["Q6item4"]] == "Pas du tout d’accord", 1,
                            ifelse(data[["Q6item4"]] == "Pas d’accord", 2,
                                   ifelse(data[["Q6item4"]] == "Neutre", 3,
                                          ifelse(data[["Q6item4"]] == "D’accord", 4,
                                                 ifelse(data[["Q6item4"]] == "Tout à fait d’accord", 5, data[["Q6item4"]])))))

#-----------------Remarque :---------------------------------------

#On convertit avec as.factor puisque les var sont nominatifs 

#------------------------------------------------------------------

#conversion of Q1

if (!is.numeric(data$Q1))
  data$Q1=as.numeric(data$Q1)

#conversion of Q2.item1

if (!is.numeric(data$Q2item1))
  data$Q2item1=as.numeric(data$Q2item1)

#conversion of Q2.item2

if (!is.numeric(data$Q2item2))
  data$Q2item2=as.numeric(data$Q2item2)

#conversion of Q2.item3

if (!is.numeric(data$Q2item3))
  data$Q2item3=as.numeric(data$Q2item3)

#conversion of Q2.item4

if (!is.numeric(data$Q2item4))
  data$Q2item4=as.numeric(data$Q2item4)

#conversion of Q3.item1

if (!is.numeric(data$Q3item1))
  data$Q3item1=as.numeric(data$Q3item1)

#conversion of Q3.item2

if (!is.numeric(data$Q3item2))
  data$Q3item2=as.numeric(data$Q3item2)

#conversion of Q3.item3

if (!is.numeric(data$Q3item3))
  data$Q3item3=as.numeric(data$Q3item3)

#conversion of Q3.item4

if (!is.numeric(data$Q3item4))
  data$Q3item4=as.numeric(data$Q3item4)

#conversion of Q4.item1

if (!is.numeric(data$Q4item1))
  data$Q4item1=as.numeric(data$Q4item1)

#conversion of Q4.item2

if (!is.numeric(data$Q4item2))
  data$Q4item2=as.numeric(data$Q4item2)

#conversion of Q4.item3

if (!is.numeric(data$Q4item3))
  data$Q4item3=as.numeric(data$Q4item3)

#conversion of Q4.item4

if (!is.numeric(data$Q4item4))
  data$Q4item4=as.numeric(data$Q4item4)

#conversion of Q5.item1

if (!is.numeric(data$Q5item1))
  data$Q5item1=as.numeric(data$Q5item1)

#conversion of Q5.item2

if (!is.numeric(data$Q5item2))
  data$Q5item2=as.numeric(data$Q5item2)

#conversion of Q5.item3

if (!is.numeric(data$Q5item3))
  data$Q5item3=as.numeric(data$Q5item3)

#conversion of Q6.item1

if (!is.numeric(data$Q6item1))
  data$Q6item1=as.numeric(data$Q6item1)

#conversion of Q6.item2

if (!is.numeric(data$Q6item2))
  data$Q6item2=as.numeric(data$Q6item2)

#conversion of Q6.item3

if (!is.numeric(data$Q6item3))
  data$Q6item3=as.numeric(data$Q6item3)

#conversion of Q6.item4

if (!is.numeric(data$Q6item4))
  data$Q6item4=as.numeric(data$Q6item4)

#------------------------------------------------------------------

# 3.2 Nettoyage des données
## 3.2.1 Traitement des données aberrantes

# Extraction des valeurs aberrantes
# $out : Composant de la liste qui contient les valeurs aberrantes

#------------------------ Démarche : -------------------------------

# Le test de "boîte à moustaches" s'applique aux variables quantitatives.
# On définit un intervalle [Q1, Q3], où Q2 est la médiane (valeur du milieu) de cet intervalle.
# On définit également le minimum et le maximum de cet intervalle.
# Si le test trouve qu'une valeur est supérieure au max ou inférieure au min :
# => La valeur est considérée comme aberrante.
# Elle est éloignée de l'intervalle, c'est-à-dire qu'elle dépasse 3 fois l'écart-type.
#-----------------------------------------------------------------

# Extraction des valeurs aberrantes à l'aide de la fonction boxplot :
boxplot.stats(data$age)$out

#----------------- Traitement des valeurs aberrantes : -------------------------------

# Aucune valeur aberrante détectée ( hors de l'intervalle).

# Les valeurs aberrantes sont traitées avec la commande suivante :
# out = boxplot.stats(data$valeur_aberrante)

# Ensuite, on utilise une boucle pour traiter les valeurs aberrantes :
# for(i in 1:length(data$valeur_aberrante)) {
#   if(data$valeur_aberrante[i] %in% out) {
#     data$valeur_aberrante[i] = NA  # Remplacer les valeurs aberrantes par NA
#   }
# }

# Déterminer la proportion des valeurs manquantes :
# Si l'effectif est valide (>= 30) :
# - Si la proportion de valeurs manquantes < seuil (5%), on les supprime.
# - Si la proportion > seuil, on les remplace par une valeur estimée.
# Si nécessaire, on estime la valeur manquante :
# data$variable = as.integer(data$variable)

#--------------------------------------------------------

## 3.2.2 Les données manquantes

# Aucune donnée manquante dans le jeu de données actuel.

# 3.3 Test de Normalité : Variables quantitatives
#--------------------- Démarche : -------------------------------------

# Le test de normalité s'applique principalement aux variables quantitatives.
# Il peut concerner les variables qualitatives si :
# - Le nombre de modalités (choix) est supérieur à 5
# - L'effectif est supérieur à 30

# L'intérêt du test de normalité est de guider le choix des tests d'hypothèses à utiliser
# lors du pré-traitement des données.

# Fonction shapiro.test() : Ce test réalise le test de Shapiro-Wilk pour la normalité.

#----------------------------------------------------------------------

# Hypothèses du test de Shapiro-Wilk :
# H0 : Il n'y a pas de différence significative entre la distribution normale et la distribution observée.
# H1 : Il existe une différence significative entre les deux distributions.

shapiro.test(data$age) 

# Résultat du test : 
# W = 0.77328, p-value = 6.515e-10 <=> p-value < 5% 
# ==> H1 est acceptée
# ==> Il y a une différence significative
# ==> L'âge ne suit pas une distribution normale.
# On vérifie donc la quasi-normalité.

# Test de quasi-normalité :
library(moments)  # Bibliothèque pour calculer des moments statistiques des distributions de données

# La distribution de l'âge est considérée comme quasi-normale si l'asymétrie et la kurtose
# sont dans l'intervalle [-3, +3].
skewness(data$age)  
# 0.9447548 appartient à l'intervalle [-3; 3]
kurtosis(data$age)  
# 5.539796 n'appartient pas à l'intervalle [-3; 3]

# ==> L'âge ne suit pas une quasi-normalité.
# ==> Nous allons donc effectuer un test non paramétrique.

# Test non paramétrique : 
wilcox.test(data$age, mu=0)

# Résultat du test :
# p-value = 3.877e-16 < 5% 
# Nous concluons que la médiane de l'âge dans notre échantillon est significativement différente de zéro.
# Nous avons suffisamment de preuves pour conclure que la médiane de l'âge n'est pas égale à zéro.

# Conclusion : La variable Âge n'est ni normale ni quasi-normale.

dens=density(data$age)
plot(dens)
hist(data$age)

#--------------------------------------------------------

# 3.4 Test de Fiabilité (test préalable, appelé test de signification des variables)
# Il existe différentes méthodes :
# ------------ Tester et retester ------------
# ------------ Test à moitié ---------------
# ------------ Le test de cohérence interne (que nous allons utiliser) ------------
# (Cela implique de calculer une forme de corrélation multiple entre différents items)

# Installation des bibliothèques nécessaires
# install.packages("psych")
# install.packages("ltm")

library(psych)
library(ltm)

# Création du dataframe avec les différentes variables des items
df = data.frame(
  data$Q1, data$Q2item1, data$Q2item2, data$Q2item3, data$Q2item4,
  data$Q3item1, data$Q3item2, data$Q3item3, data$Q3item4,
  data$Q4item1, data$Q4item2, data$Q4item3, data$Q4item4,
  data$Q5item1, data$Q5item2, data$Q5item3,
  data$Q6item1, data$Q6item2, data$Q6item3, data$Q6item4
)

# Calcul de l'alpha de Cronbach pour mesurer la fiabilité interne
cronbach.alpha(df)

# Résultats :
# Items: 20
# Unités de l'échantillon: 82
# Alpha de Cronbach : 0.756

# Interprétation de l'alpha de Cronbach selon les critères de Cronbach :
# - 0.5 - 0.6 : Fiabilité modérée
# - 0.6 - 0.7 : Fiabilité moyenne
# - 0.7+ : Bonne fiabilité
# - 0.8+ : Très bonne fiabilité
# - 0.9+ : Excellente fiabilité

# Conclusion : Dans notre cas, l'alpha de Cronbach est de 0.756, ce qui indique une bonne fiabilité.


###############################################################################
# Step 4: Prétraitement des données
## 4.1 Statistiques descriptives univariées :

# Résumé des statistiques descriptives :
summary(data)

#** Variable "age" quantitative **

# Histogramme de la variable âge :
hist(data$age)
# Affichage de l'histogramme de l'âge (fréquence) : l'âge de 20 ans est le plus dominant.

# Indicateur de dispersion (écart-type) :
sd(data$age)  
# Écart-type = 0.9532903
# La plupart des âges se situent à environ ±0.95 ans de la moyenne (20.17).

# Indicateur de dispersion (variance) :
var(data$age)
# Variance = 0.9087624
# L'âge dans notre échantillon est dispersé autour de la moyenne avec une certaine ampleur.

# Indicateur de dispersion (étendue) :
max(data$age) - min(data$age)
# Étendue = 5
# La plage d'âge couverte dans l'échantillon est de 5 ans, allant de 18 à 23 ans.

#** Variables qualitatives **

# Fréquence et proportionnalité :
plot(data$Genre)
prop.table(table(data$Genre)) 
# Femmes : 0.804878 / Hommes : 0.195122

plot(data$Niveau)
prop.table(table(data$Niveau)) 
# 1re année cycle ingénieur : 0.78048780

plot(data$Filiere)
prop.table(table(data$Filiere))
# Génie Industriel : 0.47560976

# Fréquence des réponses pour les différentes questions (Q1 à Q6) :
# Q1 :
plot(data$Q1) 
prop.table(table(data$Q1))
# Plus fréquent : 4 (46%)

# Q2 :
plot(data$Q2item1) 
prop.table(table(data$Q2item1))
# Plus fréquent : 4 (47%)
plot(data$Q2item2) 
prop.table(table(data$Q2item2))
# Plus fréquent : 4 (47%)
plot(data$Q2item3) 
prop.table(table(data$Q2item3))
# Plus fréquent : 4 (46%)
plot(data$Q2item4) 
prop.table(table(data$Q2item4))
# Plus fréquent : 5 (43%)

# Q3 :
plot(data$Q3item1) 
prop.table(table(data$Q3item1))
# Plus fréquent : 4 (46%)
plot(data$Q3item2) 
prop.table(table(data$Q3item2))
# Plus fréquent : 4 (52%)
plot(data$Q3item3) 
prop.table(table(data$Q3item3))
# Plus fréquent : 4 (42%)
plot(data$Q3item4) 
prop.table(table(data$Q3item4))
# Plus fréquent : 3 (30%)

# Q4 :
plot(data$Q4item1) 
prop.table(table(data$Q4item1))
# Plus fréquent : 4 (37%)
plot(data$Q4item2) 
prop.table(table(data$Q4item2))
# Plus fréquent : 4 (39%)
plot(data$Q4item3) 
prop.table(table(data$Q4item3))
# Plus fréquent : 4 (40%)
plot(data$Q4item4) 
prop.table(table(data$Q4item4))
# Plus fréquent : 4 (46%)

# Q5 :
plot(data$Q5item1) 
prop.table(table(data$Q5item1))
# Plus fréquent : 5 (50%)
plot(data$Q5item2) 
prop.table(table(data$Q5item2))
# Plus fréquent : 4 (41%)
plot(data$Q5item3) 
prop.table(table(data$Q5item3))
# Plus fréquent : 4 (41%)

# Q6 :
plot(data$Q6item1) 
prop.table(table(data$Q6item1))
# Plus fréquent : 4 (42%)
plot(data$Q6item2) 
prop.table(table(data$Q6item2))
# Plus fréquent : 4 (47%)
plot(data$Q6item3) 
prop.table(table(data$Q6item3))
# Plus fréquent : 5 (50%)
plot(data$Q6item4) 
prop.table(table(data$Q6item4))
# Plus fréquent : 5 (48%)


# Step 4.2 Statistiques descriptives bivariées : 
## 4.2.1 Test d'hypothèses
# ------------------ Demarche : --------------------------------------------------------

# Pour tester les hypothèses, il faut d'abord tester les pré-requis (sous-hypothèses)
# En tenant compte des cas où l'on a : la différence entre l'effectif et l'égalité.
# 1- Formuler les hypothèses statistiques (Ho, H1).
# 2- Appliquer la règle d'or :
#    Si p-value < 5% : H1 : Il y a une différence significative.
#    Si p-value > 5% : H1 : Il n'y a pas de différence significative.
#    Le "?" peut être une différence ou une association.

# Types de tests : 

# Comparaison : 
# 1. Une seule variable qualitative => test de fréquence (test de Khi-deux).
# 2. Deux variables : qualitative + quantitative => test de comparaison de moyenne.
#    - Si la variable quantitative suit une loi normale, on utilise le test "t test" ou "test de Student".
#    - Si la variable ne suit pas la loi normale, on compare les rangs par la médiane avec le test "Wilcox".
#    N.B : Si la variable est qualitative avec 2 modalités, on utilise `wilcox.test(x, y)`. Si elle a plus de 2 modalités, on utilise `kruskal.test(x, y)`.

# Association :
# 1. Deux variables ou plus :
#    - Qualitative + qualitative : test de correspondance ou test de Khi-deux.
#    - Quantitative + quantitative : test de corrélation.
#    - Si les deux variables quantitatives suivent une loi normale : test paramétrique, test de Pearson.
#    - Si l'une des variables n'est pas normale : test non paramétrique, test de Spearman.

# Test de représentativité : Test pour les variables qualitatives

# Test de Khi-deux : Comparaison des effectifs théoriques et observés.
# H0 : Il n'y a pas de différence significative.
# H1 : Il y a une différence significative.

# Test pour "Genre" :
chisq.test(table(data$Genre)) 
table(data$Genre)
# p-value = 3.36e-08 <=> p-value < 5% 
# => Il y a une différence significative entre la distribution de l'échantillon et la distribution de la variable "Genre".
# Notre échantillon n'est pas représentatif par rapport au genre.

# Test pour "Filière" :
chisq.test(table(data$Filiere)) 
table(data$Filiere)
# p-value = 1.504e-14 <=> p-value < 5% 
# => Il y a une différence significative entre la distribution de l'échantillon et la distribution de la variable "Filière".
# Notre échantillon n'est pas représentatif par rapport à la Filière d'étude.

# Test pour "Niveau" :
chisq.test(table(data$Niveau)) 
table(data$Niveau)
# p-value < 2.2e-16 <=> p-value < 5% 
# => Il y a une différence significative entre la distribution de l'échantillon et la distribution de la variable "Niveau".
# Notre échantillon n'est pas représentatif par rapport au niveau d'éducation.

# Tests d'association

# H0 : Il n'y a pas d'association significative entre "Genre" et "Niveau".
# H1 : Il y a une association significative entre "Genre" et "Niveau".
chisq.test(table(data$Genre, data$Niveau))
table(data$Genre, data$Niveau)
# p-value = 0.4597 > 5% 
# => Il n'y a pas d'association significative entre "Genre" et "Niveau".

# H0 : Il n'y a pas d'association significative entre "Genre" et "Filière".
# H1 : Il y a une association significative entre "Genre" et "Filière".
chisq.test(table(data$Genre, data$Filiere))
table(data$Genre, data$Filiere)
# p-value = 0.5749 > 5% 
# => Il n'y a pas d'association significative entre "Genre" et "Filière".

# Tests de comparaison : 
# Nous allons utiliser un test non paramétrique car la variable "âge" n'est ni normale ni quasi-normale.

# Test entre "Age" et "Genre" :
table(data$age, data$Genre)
# Test non paramétrique : 
# H0 : Il n'y a pas de différence significative entre "Age" et "Genre".
# H1 : Il y a une différence significative entre "Age" et "Genre".
wilcox.test(table(data$age, data$Genre))
# p-value = 0.005761 < 5% 
# => H1 accepté.
# => Il y a une différence significative entre "Genre" et "Age".

# Test entre "Age" et "Niveau" :
table(data$age, data$Niveau)
# Test non paramétrique : 
# H0 : Il n'y a pas de différence significative entre "Age" et "Niveau".
# H1 : Il y a une différence significative entre "Age" et "Niveau".
kruskal.test(data$age, data$Niveau)
# p-value = 3.039e-05 < 5% 
# => H1 accepté.
# => Il y a une différence significative entre "Niveau" et "Age".

# Test entre "Age" et "Filière" :
table(data$age, data$Filiere)
# Test non paramétrique : 
# H0 : Il n'y a pas d'association significative entre "Age" et "Filière".
# H1 : Il y a une association significative entre "Age" et "Filière".
kruskal.test(data$age, data$Filiere)
# p-value = 0.09381 > 5% 
# => H0 accepté.
# => Il n'y a pas d'association significative entre "Filière" et "Age".

#---------------------------------------------------------------

# Test des hypothèses

#-------------------------H1:-------------------------------------------

# Variables : Q1 (mesure de familiarité) et Q2 (perception des avantages de l’IA).
#H0: Le niveau de familiarité avec les outils numériques n'influence pas la perception des avantages de l'IA.
#H1: Le niveau de familiarité avec les outils numériques influence la perception des avantages de l'IA.

# Test du chi-carré pour chaque item de Q2
chisq.test(table(data$Q1, data$Q2item1))  # Item 1
# p-value = 0.4759 > 5%
chisq.test(table(data$Q1, data$Q2item2))  # Item 2
# p-value = 0.453 > 5%
chisq.test(table(data$Q1, data$Q2item3))  # Item 3
# p-value = 0.3162 > 5%
chisq.test(table(data$Q1, data$Q2item4))  # Item 4
# p-value = 0.6025 > 5%

# Ajout d'un test exact de Fisher pour chaque item
fisher.test(table(data$Q1, data$Q2item1))
# p-value = 0.3558 > 5%
fisher.test(table(data$Q1, data$Q2item2))
# p-value = 0.1692 > 5%
fisher.test(table(data$Q1, data$Q2item3))
# p-value = 0.2086 > 5%
fisher.test(table(data$Q1, data$Q2item4))
# p-value = 0.6617 > 5%

#----------------------Interprétation :----------------------------------
# Tous les tests montrent p-value > 0,05. on accepte H0
# Il n'existe pas de relation significative entre le niveau de familiarité (Q1) et 
# la perception d’un avantage spécifique de l'IA (Q2).


#-------------------------H2:-------------------------------------------
#H0: La personnalisation des contenus (Q2item1) n'est pas perçue comme l’avantage principal, 
# comparé aux autres items de Q2.
#H1 : La personnalisation des contenus (Q2item1) est perçue comme l’avantage principal, 
# comparé aux autres items de Q2.

# Test du chi-carré et Fisher pour comparer Q2item1 avec les autres items
chisq.test(table(data$Q2item1, data$Q2item2))
# p-value = 0.2479 > 5%
fisher.test(table(data$Q2item1, data$Q2item2))
# p-value = 0.1639 > 5%

chisq.test(table(data$Q2item1, data$Q2item3))
# p-value = 0.08179 > 5%
fisher.test(table(data$Q2item1, data$Q2item3))
# p-value = 0.03737 < 5%

chisq.test(table(data$Q2item1, data$Q2item4))
# p-value = 0.05851 > 5%
fisher.test(table(data$Q2item1, data$Q2item4))
# p-value = 0.009212 < 5%

# Distribution des réponses pour Q2
table(data$Q2item1)
table(data$Q2item2)
table(data$Q2item3)
table(data$Q2item4)

#----------------------Interprétation :----------------------------------
#Comparaison entre Q2item1 et Q2item2 :Aucune différence significative entre la perception de la personnalisation (Q2item1) et celle de Q2item2.
#Comparaison entre Q2item1 et Q2item3 :Une différence significative est observée selon le test exact de Fisher, suggérant que Q2item1 et Q2item3 sont perçus différemment.
#Comparaison entre Q2item1 et Q2item4 :Une différence significative est observée entre Q2item1 et Q2item4.

#Analyse des distributions :
#Q2item1 (Personnalisation des contenus) : La réponse la plus fréquente est 4 (47%), indiquant un accord majoritaire mais pas absolu.
#Q2item2 : Même tendance que Q2item1 avec un accord à 4 (47%).
#Q2item3 : La majorité des réponses est également à 4 (46%), mais les différences observées avec Q2item1 sont significatives selon Fisher.
#Q2item4 : La réponse dominante est 5 (43%), montrant un niveau d’accord légèrement plus élevé comparé aux autres items.

#Conclusion: Les tests montrent que Q2item1 (personnalisation des contenus) diffère significativement de Q2item3 et Q2item4 (Fisher), mais pas de Q2item2. 
#Avec 43% de réponses à 5, Q2item4 est légèrement plus perçu comme avantage principal, suggérant que la personnalisation est appréciée mais pas dominante.

#-------------------------H3:-------------------------------------------
# H0 : L’optimisation des programmes éducatifs grâce à l’analyse des données n'influence pas positivement les méthodes d’apprentissage traditionnelles, il y on a d'autres.
# H1 : L’optimisation des programmes éducatifs grâce à l’analyse des données influence positivement les méthodes d’apprentissage traditionnelles.

# Comparaison entre Q3item3 et Q1
chisq.test(table(data$Q1, data$Q3item3))
# p-value = 0.2182 > 5%
fisher.test(table(data$Q1, data$Q3item3))
# p-value = 0.1216 > 5%

# Comparaison entre les items de Q3
chisq.test(table(data$Q3item3, data$Q3item1))
# p-value = 0.1035 > 5%
fisher.test(table(data$Q3item3, data$Q3item1))
# p-value = 0.08084 > 5%

chisq.test(table(data$Q3item3, data$Q3item2))
# p-value = 0.05676 > 5%
fisher.test(table(data$Q3item3, data$Q3item2))
# p-value = 0.01136 < 5%

chisq.test(table(data$Q3item3, data$Q3item4))
# p-value = 1.841e-05 < 5%
fisher.test(table(data$Q3item3, data$Q3item4),simulate.p.value=TRUE)
# p-value = 0.001499 < 5%

# Distribution des réponses pour Q3
table(data$Q3item3)
table(data$Q3item1)
table(data$Q3item2)
table(data$Q3item4)

#----------------------Interprétation :----------------------------------
#Q3item3 (analyse des données pour optimiser les programmes éducatifs) ne diffère pas significativement de Q1 (familiarité numérique), 
#avec une p-value > 5% (chi-carré et Fisher).

#Comparaisons avec les autres items de Q3 :

#Pas de différence significative avec Q3item1 (fisher: p = 0.08084 > 5%).
#Différence significative avec Q3item2 (fisher: p = 0.01136 < 5%).
#Différence hautement significative avec Q3item4 (fisher: p = 0.001499 < 5%).

#Selon les proportions :
  
#Q3item2 (adaptation aux besoins des étudiants) reste le plus dominant (52% à 4).
#Q3item1 (apprentissage plus efficace) et Q3item3 (analyse des données) sont appréciés à des niveaux comparables (46% et 42% à 4 respectivement), 
#mais Q3item1 a une proportion légèrement plus forte pour la note 5 (40%) que Q3item3.

#Q3item4 (renforcement des interactions humaines) est le moins fréquent (30% à 3).

#Conclusion :
  
#Les participants considèrent Q3item2 comme l’avantage clé.
#Q3item3 est également bien perçu mais reste légèrement moins dominant que Q3item1 en termes d’apprentissage plus efficace.
#Enfin, l’analyse des données (Q3item3) est jugée significativement plus importante que le renforcement des interactions humaines (Q3item4).

#-------------------------H4:-------------------------------------------
# H0 : Les préoccupations éthiques sur la confidentialité des données (Q4item3) 
# ne sont pas les principales inquiétudes.
# H1 : Les préoccupations éthiques sur la confidentialité des données (Q4item3) 
# sont les principales inquiétudes.

# Comparaison entre Q4item3 et les autres items
chisq.test(table(data$Q4item3, data$Q4item1))
# p-value = 0.585 > 5%
chisq.test(table(data$Q4item3, data$Q4item2))
# p-value = 0.03159 < 5%
chisq.test(table(data$Q4item3, data$Q4item4))
# p-value = 0.0004847 < 5%

# Distribution des réponses pour Q4
table(data$Q4item3)
table(data$Q4item1)
table(data$Q4item2)
table(data$Q4item4)

#----------------------Interprétation :----------------------------------
# Les préoccupations sur la confidentialité des données (Q4item3) diffèrent significativement de Q4item2 et Q4item4.
# Cependant, aucune différence significative avec Q4item1.
#Distribution des réponses :
  
#Q4item3 (confidentialité des données) : La réponse la plus fréquente est 4 (33 réponses, 40%), suivie de 3 (23 réponses, 28%). 
#Cela montre un niveau d'accord globalement élevé.

#Q4item1 (transparence des algorithmes) : La réponse la plus fréquente est également 4 (31 réponses, 37%), 
#mais 26 réponses à 5 (31%) montrent un niveau d'accord global légèrement plus fort.

#Q4item2 (préjugés dans l'IA) : Bien que 4 (32 réponses, 39%) soit dominant, un nombre plus élevé de réponses à 3 (31 réponses, 38%) 
#indique une perception un peu moins favorable.

#Q4item4 (dépendance excessive à la technologie) : Réponses majoritairement positives avec 4 (38 réponses, 46%) et 5 (24 réponses, 29%), 
#ce qui confirme son statut dominant.

#Conclusion :

#Les participants jugent les préoccupations sur la confidentialité des données (Q4item3) importantes, mais elles ne sont pas perçues comme la préoccupation principale.
#La dépendance excessive à la technologie (Q4item4) est significativement plus préoccupante, avec des niveaux d'accord plus élevés.
#Les réponses indiquent également une bonne reconnaissance de l'importance de la transparence des algorithmes (Q4item1).


#-------------------------H5:-------------------------------------------
# H0 : Les étudiants ne préfèrent pas un rôle de soutien administratif (Q5item1).
# H1 : Les étudiants préfèrent un rôle de soutien administratif (Q5item1) à d'autres rôles.

# Comparaison entre Q5item1 et les autres items
chisq.test(table(data$Q5item1, data$Q5item2))
# p-value = 0.1194 > 5%
fisher.test(table(data$Q5item1, data$Q5item2))
# p-value = 0.0152 < 5%

chisq.test(table(data$Q5item1, data$Q5item3))
# p-value = 0.002141 < 5%
fisher.test(table(data$Q5item1, data$Q5item3))
# p-value = 0.003631 < 5%

# Distribution des réponses pour Q5
table(data$Q5item1)
table(data$Q5item2)
table(data$Q5item3)

#----------------------Interprétation :----------------------------------
#Comparaison entre Q5item1 (soutien administratif) et les autres items :

#Avec Q5item2 (assistant pédagogique) :
  #Test du chi-carré : aucune différence significative.
  #Test de Fisher : différence significative, suggérant que les réponses entre Q5item1 et Q5item2 diffèrent modérément.
#Avec Q5item3 (facilitateur d’apprentissage) :
  #Test du chi-carré : différence significative.
  #Test de Fisher : différence confirmée, indiquant que Q5item1 est perçu différemment de Q5item3.

#Distribution des réponses :
  
#Q5item1 (soutien administratif) : La majorité des participants a répondu 5 (41 réponses, 50%), indiquant un fort accord, suivi de 4 (30 réponses, 37%). 
#Cela montre une préférence claire pour ce rôle.

#Q5item2 (assistant pédagogique) : Réponses légèrement plus dispersées, avec une majorité à 4 (34 réponses, 41%) et une proportion significative à 5 (30 réponses, 36%).

#Q5item3 (facilitateur d’apprentissage) : Bien que 4 (34 réponses, 41%) soit le plus fréquent, 5 (28 réponses, 34%) est légèrement moins dominant comparé à Q5item1.

#Conclusion :
  
#Les étudiants préfèrent significativement le rôle de soutien administratif (Q5item1) par rapport au rôle de facilitateur d’apprentissage (Q5item3).
#La différence avec assistant pédagogique (Q5item2) est modérée mais reste significative selon le test de Fisher.
#Les réponses confirment que le soutien administratif est perçu comme un rôle clé pour l’IA, avec un niveau d’accord globalement plus élevé par rapport aux autres rôles.


#-------------------------H6:-------------------------------------------
# H0 : La transparence (Q6item1) et la formation (Q6item2) ne sont pas perçues comme essentielles.
# H1 : La transparence (Q6item1) et la formation (Q6item2) sont perçues comme essentielles.

# Comparaison entre les items de Q6
chisq.test(table(data$Q6item1, data$Q6item2))
# p-value = 0.5064 > 5%
fisher.test(table(data$Q6item1, data$Q6item2))
# p-value = 0.5297 > 5%

chisq.test(table(data$Q6item1, data$Q6item3))
# p-value = 0.03909 < 5%
fisher.test(table(data$Q6item1, data$Q6item3))
# p-value = 0.1324 > 5%

chisq.test(table(data$Q6item1, data$Q6item4))
# p-value = 0.2676 > 5%
fisher.test(table(data$Q6item1, data$Q6item4))
# p-value = 0.152 > 5%

# Distribution des réponses pour Q6
table(data$Q6item1)
table(data$Q6item2)
table(data$Q6item3)
table(data$Q6item4)

#----------------------Interprétation :----------------------------------
#Comparaisons entre les items de Q6 :

#Q6item1 (transparence) vs. Q6item2 (formation) : Aucune différence significative, suggérant que les perceptions de transparence et de formation sont similaires.
#Q6item1 vs. Q6item3 (mise en œuvre pratique) : Ces résultats montrent une certaine différence, mais elle n’est pas suffisamment robuste.
#Q6item1 vs. Q6item4 (fiabilité) : Aucune différence significative, indiquant une perception similaire entre transparence et fiabilité.

#Distribution des réponses :
  
#Q6item1 (transparence) : Les réponses se concentrent majoritairement sur 4 (35 réponses, 42%) et 5 (28 réponses, 34%), 
#confirmant que la transparence est largement perçue comme importante.

#Q6item2 (formation) : La majorité des participants a répondu 4 (39 réponses, 47%) et 5 (25 réponses, 30%), 
#indiquant un fort accord similaire à celui observé pour Q6item1.

#Q6item3 (mise en œuvre pratique) : Les réponses les plus fréquentes sont 5 (41 réponses, 50%) et 4 (27 réponses, 33%), 
#ce qui montre une importance perçue encore plus forte pour cet item.

#Q6item4 (fiabilité) : Les scores les plus fréquents sont 5 (40 réponses, 48%) et 4 (28 réponses, 34%), ce qui place la fiabilité 
#comme un élément perçu également essentiel.

#Conclusion :
  
#La transparence (Q6item1) et la formation (Q6item2) sont perçues comme essentielles, sans différences significatives entre elles, soutenant l’hypothèse.
#Cependant, la mise en œuvre pratique (Q6item3) et la fiabilité (Q6item4) sont aussi fortement valorisées, avec des niveaux d’accord similaires voire supérieurs.
#Ces résultats suggèrent que bien que transparence et formation soient importantes, d'autres aspects comme la mise en œuvre pratique et la fiabilité 
#doivent également être pris en compte.



#----------------VALIDATION DES HYPOTHESES :------------------------------

#Les hypothèses formulées au début de l’étude ont été testées, et voici les résultats de la validation des hypothèses :
  #-------------------------H1:-------------------------------------------

#Hypothèse : Le niveau de familiarité avec les outils numériques influence la perception des avantages de l'IA.
#Conclusion : Il n'y a pas de lien significatif entre la familiarité avec les outils numériques et la perception des avantages de l'IA.
#-------------------------H2:-------------------------------------------

#Hypothèse : La personnalisation des contenus est perçue comme l’avantage principal.
#Conclusion : La personnalisation des contenus n'est pas perçue comme l’avantage principal. D’autres avantages sont considérés plus importants.
#-------------------------H3:-------------------------------------------

#Hypothèse : L’optimisation des programmes éducatifs grâce à l’analyse des données influence positivement les méthodes d’apprentissage traditionnelles.
#Conclusion : L’optimisation des programmes éducatifs est perçue comme un avantage clé, surpassant d'autres avantages liés aux interactions.
#-------------------------H4:-------------------------------------------

#Hypothèse : Les préoccupations éthiques sur la confidentialité des données sont les principales inquiétudes.
#Conclusion : La dépendance excessive à la technologie est une préoccupation plus importante que la confidentialité des données.
#-------------------------H5:-------------------------------------------

#Hypothèse : Les étudiants préfèrent un rôle de soutien administratif à d'autres rôles.
#Conclusion : Les étudiants préfèrent un rôle d'outil complémentaire à l’apprentissage plutôt qu'un rôle administratif.
#-------------------------H6:-------------------------------------------

#Hypothèse : La transparence et la formation sont perçues comme essentielles.
#Conclusion : La confidentialité des données et l'intégration de l'IA comme outil, plutôt que remplacement, sont perçues comme plus importantes.



#----------------Conclusion :-------------------------------------------

#En résumé, certaines hypothèses montrent des différences significatives, notamment sur l’optimisation des programmes éducatifs, les préoccupations éthiques, 
#et les rôles de l’IA. D'autres hypothèses, comme la perception des avantages, n'ont pas montré de liens significatifs.

#Nous validons ainsi les hypothèses qui ont montré des différences significatives et rejetons celles qui ne montrent pas de relations significatives.

#Les principaux obstacles sont la dépendance excessive à la technologie et la perception de l'IA comme un outil de remplacement plutôt que de soutien.

#On peut donc dire qu'une étude approfondie a été réalisée sur l'analyse des données, permettant de valider certaines hypothèses tout en rejetant 
#celles sans fondement significatif.

#------------------------------------Fin---------------------------------

#-------------------------------Realiser par :-------------------------

#El Asri Ilyas




