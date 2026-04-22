
# Charger les librairies
library(ggplot2)

# Charger le fichier 
df <- read.csv2("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", stringsAsFactors = FALSE)

# Conversion âge
df$age_estim <- as.numeric(gsub(",", ".", df$age_estim))


# Graphique
library(ggplot2)

ggplot(df, aes(x = age_estim, fill = fk_stadedev)) +
  geom_histogram(binwidth = 10, alpha = 0.6, position = "identity") +
  labs(title = "Répartition des âges selon le stade de développement",
       x = "Âge estimé",
       y = "Effectif",
       fill = "Stade de développement") +
  theme_minimal()
