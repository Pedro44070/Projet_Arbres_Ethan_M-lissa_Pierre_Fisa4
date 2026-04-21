# ==========================================================
# ANALYSE STATISTIQUE DU PATRIMOINE ARBORÉ
# ==========================================================
if(!require(corrplot)) install.packages("corrplot")
if(!require(dplyr)) install.packages("dplyr")

library(corrplot)
library(dplyr)
library(ggplot2)
# ==========================================================
# ANALYSE STATISTIQUE DU PATRIMOINE ARBORÉ
# ==========================================================

# 1. Chargement des données et nettoyage
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (1).csv", sep = ";", stringsAsFactors = FALSE)

df_clean <- df %>%
  mutate(
    age_estim = as.numeric(gsub(",", ".", as.character(age_estim))),
    tronc_diam = as.numeric(gsub(",", ".", as.character(tronc_diam))) * 100, # en cm
    haut_tot = as.numeric(gsub(",", ".", as.character(haut_tot)))
  ) %>%
  filter(
    !is.na(age_estim), age_estim > 0, age_estim < 500,
    !is.na(tronc_diam), tronc_diam > 0,
    !is.na(haut_tot), haut_tot > 0
  )

# --- A. STATISTIQUES DESCRIPTIVES (UNIVARIÉES) ---
# Comprendre la répartition de vos arbres
cat("--- RÉSUMÉ GÉNÉRAL ---\n")
print(summary(df_clean[, c("age_estim", "tronc_diam", "haut_tot")]))

# --- B. ANALYSE BIVARIÉE (PAR QUARTIER) ---
# Existe-t-il des disparités entre les quartiers ?
analyse_quartier <- df_clean %>%
  group_by(clc_quartier) %>%
  summarise(
    Nombre_Arbres = n(),
    Age_Moyen = round(mean(age_estim), 1),
    Diametre_Moyen = round(mean(tronc_diam), 1)
  ) %>%
  arrange(desc(Age_Moyen))

cat("\n--- ANALYSE PAR QUARTIER ---\n")
print(analyse_quartier)

# --- C. CORRÉLATIONS ---
# Calcul du coefficient de corrélation de Pearson
# (Plus il est proche de 1, plus le lien est fort)
matrice_cor <- cor(df_clean[, c("age_estim", "tronc_diam", "haut_tot")], use = "complete.obs")

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

# --- D. GRAPHIQUE COMPLÉMENTAIRE : Hauteur vs Diamètre ---
ggplot(df_clean, aes(x = tronc_diam, y = haut_tot, color = age_estim)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  scale_color_viridis_c() +
  labs(title = "Relation Hauteur vs Diamètre",
       subtitle = "La couleur représente l'âge",
       x = "Diamètre du tronc (cm)",
       y = "Hauteur totale (m)") +
  theme_minimal()
