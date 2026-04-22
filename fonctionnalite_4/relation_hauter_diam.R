#import librairy 
if(!require(corrplot)) install.packages("corrplot")
if(!require(dplyr)) install.packages("dplyr")

library(corrplot)
library(dplyr)
library(ggplot2)

#import de données 
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", sep = ";", stringsAsFactors = FALSE)

#création du graphique

ggplot(df, aes(x = tronc_diam, y = haut_tot, color = age_estim)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  scale_color_viridis_c() +
  labs(title = "Relation Hauteur vs Diamètre",
       subtitle = "La couleur représente l'âge",
       x = "Diamètre du tronc (cm)",
       y = "Hauteur totale (m)") +
  theme_minimal()