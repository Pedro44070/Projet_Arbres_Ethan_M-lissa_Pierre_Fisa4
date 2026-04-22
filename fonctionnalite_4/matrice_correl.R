#import librairy 
if(!require(corrplot)) install.packages("corrplot")
if(!require(dplyr)) install.packages("dplyr")

library(corrplot)
library(dplyr)
library(ggplot2)

# 1. Chargement des données et nettoyage
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", sep = ";", stringsAsFactors = FALSE)


# ---  CORRÉLATIONS ---
# Calcul du coefficient de corrélation de Pearson

matrice_cor <- cor(df[, c("age_estim", "tronc_diam", "haut_tot")], use = "complete.obs")

cat("\n--- MATRICE DE CORRÉLATION ---\n")
print(round(matrice_cor, 2))

# Visualisation graphique de la corrélation
corrplot(matrice_cor, 
         method = "ellipse", 
         type = "upper", 
         tl.col = "black", 
         addCoef.col = "black",
         title = "\nLien entre Age, Diamètre et Hauteur",
         mar = c(0,0,2,0))

