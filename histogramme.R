# ==========================================================
# ANALYSE COMPLÈTE DU PATRIMOINE ARBORÉ
# ==========================================================

# 1. Chargement des bibliothèques
library(ggplot2)

# 2. Importation et Nettoyage
# (Note : Ajustez le chemin du fichier si nécessaire)
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (1).csv", sep = ";", stringsAsFactors = FALSE)
# ==========================================================
# ANALYSE COMPLÈTE DU PATRIMOINE ARBORÉ (Version Corrigée)
# ==========================================================

# Conversion numérique sécurisée
df$age_estim <- as.numeric(gsub(",", ".", as.character(df$age_estim)))
df$tronc_diam <- as.numeric(gsub(",", ".", as.character(df$tronc_diam)))

# FILTRAGE CRITIQUE pour éviter l'erreur 'depth' :
# On retire les NA, les âges à 0, et on limite aux données cohérentes
df_clean <- df[!is.na(df$age_estim) & df$age_estim > 0 & df$age_estim < 500, ]
df_clean <- df_clean[!is.na(df_clean$tronc_diam) & df_clean$tronc_diam > 0, ]

# Conversion du diamètre en cm (si le fichier est en mètres)
df_clean$tronc_diam <- df_clean$tronc_diam * 100

# Nettoyage des quartiers
df_clean$clc_quartier <- trimws(df_clean$clc_quartier)
df_clean <- df_clean[df_clean$clc_quartier != "", ]

# 3. Création des classes d'âge
df_clean$classe_age <- cut(df_clean$age_estim, 
                           breaks = c(0, 10, 20, 30, 50, 100, 500), 
                           labels = c("0-10 ans", "10-20 ans", "20-30 ans", "30-50 ans", "50-100 ans", "100 ans +"),
                           right = FALSE)

# ==========================================================
# GRAPHIQUE 1 : Corrélation Continue (Nuage de points + Courbe)
# Cette méthode évite l'erreur 'depth' en travaillant sur les points bruts
# ==========================================================
g1 <- ggplot(df_clean, aes(x = age_estim, y = tronc_diam)) +
  # On ajoute un léger "jitter" pour voir les points superposés
  geom_jitter(aes(color = classe_age), alpha = 0.4, size = 1.5) +
  # Courbe de tendance (loess pour la souplesse ou lm pour une droite)
  geom_smooth(method = "loess", color = "black", fill = "gray", alpha = 0.2) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Corrélation entre l'âge et le diamètre du tronc",
       subtitle = "La courbe grise montre la tendance moyenne de croissance",
       x = "Âge estimé (années)",
       y = "Diamètre du tronc (cm)",
       color = "Tranche d'âge") +
  theme_minimal()

# ==========================================================
# GRAPHIQUE 2 : Répartition par Quartier (Histogramme empilé)
# ==========================================================
g2 <- ggplot(df_clean, aes(y = clc_quartier, fill = classe_age)) +
  geom_bar() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Structure d'âge par quartier",
       x = "Nombre d'arbres",
       y = "Quartier",
       fill = "Âge") +
  theme_minimal()

# Affichage des graphiques
print(g1)
print(g2)
