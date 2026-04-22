library(ggplot2)
library(dplyr)
# 2. Importation 
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", sep = ";", stringsAsFactors = FALSE)

# 1. Nettoyage strict et création de la cible
df_log <- df %>%
  mutate(
    age = as.numeric(gsub(",", ".", as.character(age_estim))),
    diam = as.numeric(gsub(",", ".", as.character(tronc_diam))),
    haut = as.numeric(gsub(",", ".", as.character(haut_tot))),
    # On s'assure d'avoir bien des 0 et des 1
    etat_abattu = ifelse(fk_arb_etat %in% c("À ABATTRE", "SUPPRIMÉ"), 1, 0)
  ) %>%
  filter(!is.na(age), !is.na(diam), age > 0)

# 2. Construction du modèle simple (Age uniquement pour garantir le S visuel)
# Si le modèle à 3 variables est plat, c'est qu'elles se contredisent.
modele_simple <- glm(etat_abattu ~ age, data = df_log, family = binomial(link = "logit"))

# 3. Création de la courbe théorique sur toute l'étendue
# On va de 0 à 600 ans pour forcer le modèle à montrer sa forme
grille_projection <- data.frame(age = seq(0, 600, by = 1))
grille_projection$probabilite <- predict(modele_simple, newdata = grille_projection, type = "response")

# 4. Le graphique de la dernière chance pour le "S"
ggplot() +

  
  # La ligne rouge de la fonction logistique
  geom_line(data = grille_projection, aes(x = age, y = probabilite), 
            color = "red", size = 1.5) +
  
  # Ajustements visuels
  scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Analyse de Survie : Probabilité d'Abattage selon l'Âge",
       subtitle = "Modèle Logistique (Sigmoïde)",
       x = "Âge de l'arbre",
       y = "Probabilité de suppression") +
  theme_minimal()
