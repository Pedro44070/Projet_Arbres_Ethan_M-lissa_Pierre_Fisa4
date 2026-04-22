# 1. Chargement des bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. Importation des données
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", 
               sep = ";", stringsAsFactors = FALSE)

# 3. Préparation rapide (Conversion en numérique pour les calculs)
df_stats <- df %>%
  mutate(
    age_estim = as.numeric(as.character(age_estim)),
    tronc_diam = as.numeric(as.character(tronc_diam)),
    haut_tot = as.numeric(as.character(haut_tot))
  )

# ==========================================================
# PARTIE A : STATISTIQUES DESCRIPTIVES (UNIVARIÉES)
# ==========================================================

cat("--- RÉSUMÉ GLOBAL DU PATRIMOINE ---\n")
# Résumé général (Min, Max, Moyenne, Médiane) pour les variables numériques
print(summary(df_stats[, c("age_estim", "tronc_diam", "haut_tot")]))

cat("\n--- TOP 5 DES ESSENCES (DIVERSITÉ) ---\n")
# Compte le nombre d'arbres par essence
top_essences <- df_stats %>%
  count(nom, sort = TRUE) %>%
  head(5)
print(top_essences)

cat("\n--- RÉPARTITION PAR ÉTAT DE SANTÉ ---\n")
# Pourcentage d'arbres en place vs supprimés
etat_sante <- df_stats %>%
  count(fk_arb_etat) %>%
  mutate(pourcentage = n / sum(n) * 100)
print(etat_sante)

# ==========================================================
# PARTIE B : ANALYSE PAR GROUPE (QUARTIERS)
# ==========================================================

# ==========================================================
# PARTIE B : ANALYSE PAR GROUPE (QUARTIERS) - CORRIGÉE
# ==========================================================

cat("\n--- STATISTIQUES PAR QUARTIER ---\n")

# Calcul de la moyenne d'âge et du nombre d'arbres par quartier
stats_quartier <- df_stats %>%
  group_by(clc_quartier) %>%
  summarise(
    Nombre_Arbres = n(),
    Age_Moyen = mean(age_estim, na.rm = TRUE),
    Diametre_Moyen = mean(tronc_diam, na.rm = TRUE),
    # On peut aussi ajouter la hauteur moyenne pour être complet
    Hauteur_Moyenne = mean(haut_tot, na.rm = TRUE)
  ) %>%
  # On classe par les quartiers les plus arborés
  arrange(desc(Nombre_Arbres))

# Affichage des résultats
print(stats_quartier)
# ==========================================================
# PARTIE C : ANALYSE BIVARIÉE (RELATIONS)
# ==========================================================

cat("\n--- ANALYSE DE CORRÉLATION (Pearson) ---\n")
# Test de corrélation entre l'âge et le diamètre
cor_age_diam <- cor.test(df_stats$age_estim, df_stats$tronc_diam, use = "complete.obs")
cat("Corrélation Age/Diamètre :", round(cor_age_diam$estimate, 2), "\n")

# Test de corrélation entre l'âge et la hauteur
cor_age_haut <- cor.test(df_stats$age_estim, df_stats$haut_tot, use = "complete.obs")
cat("Corrélation Age/Hauteur :", round(cor_age_haut$estimate, 2), "\n")

# ==========================================================
# PARTIE D : ÉTUDE DE RÉGRESSION LINÉAIRE
# ==========================================================

# Modèle pour prédire le diamètre en fonction de l'âge
modele_regression <- lm(tronc_diam ~ age_estim, data = df_stats)

cat("\n--- RÉSULTATS DE LA RÉGRESSION (Diamètre ~ Age) ---\n")
print(summary(modele_regression))
