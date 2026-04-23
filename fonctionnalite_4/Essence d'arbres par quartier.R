# Projet Big Data - Identité végétale des quartiers
# Trinôme 4

library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

df <- read.csv("C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv", sep = ";", stringsAsFactors = FALSE)

# On exclut les valeurs non renseignées pour ne pas fausser l'analyse
valeurs_a_exclure <- c("RAS", "INCONNU", "Inconnu", "inconnu", "NC", "nc", "Autre", "autre", "")

df_clean <- df %>%
  filter(
    fk_arb_etat == "EN PLACE",
    !is.na(nom),
    !(trimws(nom) %in% valeurs_a_exclure),
    !is.na(clc_quartier),
    !(trimws(clc_quartier) %in% c("", "nan", "NA"))
  ) %>%
  mutate(
    nom = trimws(nom),
    clc_quartier = trimws(clc_quartier)
  )

top_quartiers <- df_clean %>%
  count(clc_quartier, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(clc_quartier)

top_essences <- df_clean %>%
  count(nom, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(nom)

df_croise <- df_clean %>%
  filter(clc_quartier %in% top_quartiers, nom %in% top_essences) %>%
  group_by(clc_quartier, nom) %>%
  summarise(nb_arbres = n(), .groups = "drop") %>%
  # Calcul du pourcentage au sein du quartier
  group_by(clc_quartier) %>%
  mutate(pourcentage = round(nb_arbres / sum(nb_arbres) * 100, 1)) %>%
  ungroup()


# GRAPHIQUE 1 : Barres empilées (avec pourcentages)

graphique_barres <- ggplot(df_croise, aes(x = reorder(clc_quartier, -nb_arbres, sum), y = nb_arbres, fill = nom)) +
  geom_bar(stat = "identity", position = "fill", color = "white", linewidth = 0.2) +
  # Affichage des pourcentages s'ils sont supérieurs à 3% pour la lisibilité
  geom_text(
    aes(label = ifelse(pourcentage > 3, paste0(pourcentage, "%"), ""), y = nb_arbres), 
    position = position_fill(vjust = 0.5),
    size = 3.5, 
    color = "grey15", 
    fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  labs(
    title = "Identité végétale des quartiers",
    subtitle = "Répartition des 10 essences principales",
    x = "",
    y = "Proportion d'arbres",
    fill = "Essence"
  ) +
  theme_minimal(base_size = 11)

print(graphique_barres)
ggsave("graphique_barres_pct.png", plot = graphique_barres, width = 12, height = 6)


# GRAPHIQUE 2 : Carte des essences par quartier (Heatmap)

graphique_heatmap <- ggplot(df_croise, aes(x = nom, y = reorder(clc_quartier, nb_arbres, sum), fill = nb_arbres)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = nb_arbres), size = 3.2, color = "grey20") +
  scale_fill_gradient(low = "#f7fcf5", high = "#00441b", name = "Nb d'arbres") +
  labs(
    title = "Présence des essences par quartier",
    x = "Essence",
    y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(graphique_heatmap)
ggsave("graphique_heatmap.png", plot = graphique_heatmap, width = 11, height = 7)



# SYNTHÈSE : Essence dominante par quartier

essence_dominante <- df_croise %>%
  group_by(clc_quartier) %>%
  slice_max(order_by = nb_arbres, n = 1) %>%
  select(clc_quartier, essence_dominante = nom, nb_arbres, pourcentage) %>%
  arrange(desc(nb_arbres))

print(essence_dominante)

# Le test de Chi-2 se fait sur les effectifs BRUTS (pas les pourcentages)
tableau_contingence <- table(df_croise$clc_quartier, df_croise$nom)

# 2. RÉALISATION DU TEST
test_chi2 <- chisq.test(tableau_contingence)

# 3. AFFICHAGE DES RÉSULTATS

print(test_chi2)

# Interprétation automatique
p_val <- test_chi2$p.value