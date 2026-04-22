
# 1. Chargement des bibliothèques
library(ggplot2)

# 2. Importation 

df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", sep = ";", stringsAsFactors = FALSE)


# 3. Création des classes d'âge
df$classe_age <- cut(df_clean$age_estim, 
                           breaks = c(0, 10, 20, 30, 50, 100, 500), 
                           labels = c("0-10 ans", "10-20 ans", "20-30 ans", "30-50 ans", "50-100 ans", "100 ans +"),
                           right = FALSE)

# ==========================================================
# GRAPHIQUE 1 : Corrélation entre l'age et le diamètre du tronc
# ==========================================================


g1 <- ggplot(df, aes(x = age_estim, y = tronc_diam)) +
  geom_jitter(aes(color = classe_age), alpha = 0.4, size = 1.5) +
  geom_smooth(method = "loess", color = "black", fill = "gray", alpha = 0.2) +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "Corrélation entre l'âge et le diamètre du tronc",
       subtitle = "La courbe grise montre la tendance moyenne de croissance",
       x = "Âge estimé (années)",
       y = "Diamètre du tronc (cm)",
       color = "Tranche d'âge") +
  theme_minimal()

# ==========================================================
# GRAPHIQUE 2 : Répartition par Quartier 
# ==========================================================
g2 <- ggplot(df, aes(y = clc_quartier, fill = classe_age)) +
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
