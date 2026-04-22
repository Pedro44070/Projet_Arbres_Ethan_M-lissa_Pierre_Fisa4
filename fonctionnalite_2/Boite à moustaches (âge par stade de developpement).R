# Projet Big Data - Trinôme 4
# Boîte à moustaches : Âge selon le stade de développement

library(readxl)
library(dplyr)

# 1. Chargement des données
df <- read_excel("C:/Users/PC/Downloads/Data.xlsx")

# 2. Nettoyage et préparation
df_clean <- df %>%
  mutate(
    # On met tout en minuscules pour éviter les doublons dus à la casse
    fk_stadedev = tolower(trimws(fk_stadedev)),
    age_estim   = as.numeric(age_estim)
  ) %>%
  # On garde uniquement les stades connus et les âges cohérents (on exclut les valeurs aberrantes)
  filter(
    fk_stadedev %in% c("jeune", "adulte", "vieux", "senescent"),
    !is.na(age_estim), 
    age_estim > 0, 
    age_estim < 500
  )

# On force l'ordre des catégories pour que le graphique soit logique (du plus jeune au plus vieux)
df_clean$fk_stadedev <- factor(df_clean$fk_stadedev, levels = c("jeune", "adulte", "vieux", "senescent"))

# 3. Création et sauvegarde du graphique
# On prépare le fichier image PNG
png("boxplot_age_stade.png", width = 800, height = 600)

# Tracé de la boîte à moustaches
boxplot(
  age_estim ~ fk_stadedev,
  data  = df_clean,
  col   = c("#a8d5a2", "#f4c542", "#e07b39", "#c0392b"),
  main  = "Distribution de l'âge par stade de développement",
  xlab  = "Stade de développement",
  ylab  = "Âge estimé (années)"
)

# Ajout d'un losange rouge pour indiquer la moyenne sur chaque boîte
moyennes <- tapply(df_clean$age_estim, df_clean$fk_stadedev, mean)
points(1:4, moyennes, pch = 18, col = "red", cex = 1.5)
legend("topleft", legend = "Moyenne", pch = 18, col = "red")

# On ferme et on enregistre l'image
dev.off()