# ══════════════════════════════════════════════════════════════════════════════
#  ÉTUDE QUALITATIVE – Identité végétale des quartiers de Saint-Quentin
#  Trinôme 4 : Pierre SICOT · Ethan HEURTIN · Mélissa BOUANCHAUD
#  ISEN Nantes – FISA4 – 2026
#
#  Objectif : Croiser les quartiers de la ville avec les essences d'arbres
#             pour identifier l'identité végétale de chaque secteur.
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Installation des packages (uniquement si non déjà installés) ───────────
if (!require(dplyr))   install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(scales))  install.packages("scales")       # formatage des axes
if (!require(RColorBrewer)) install.packages("RColorBrewer")

# ── 2. Chargement des librairies ──────────────────────────────────────────────
library(dplyr)          # manipulation de données
library(ggplot2)        # graphiques
library(scales)         # pourcentages sur les axes
library(RColorBrewer)   # palettes de couleurs

# ── 3. Import du fichier CSV ──────────────────────────────────────────────────
# file.choose() ouvre une fenêtre Windows pour sélectionner le fichier
df <- read.csv(
  file             = "C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv",     # sélectionne Patrimoine_Arbore_Nettoye.csv
  sep              = ";",
  header           = TRUE,
  encoding         = "UTF-8",
  stringsAsFactors = FALSE
)

cat("✅ Fichier chargé :", nrow(df), "lignes,", ncol(df), "colonnes\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. NETTOYAGE DES DONNÉES
# ══════════════════════════════════════════════════════════════════════════════
# On ne garde que :
#   - les arbres actuellement EN PLACE (ni abattus, ni supprimés)
#   - ceux dont l'essence est clairement renseignée (pas de RAS, INCONNU, vide)
#   - ceux dont le quartier est correctement rempli
# ══════════════════════════════════════════════════════════════════════════════

# Liste des valeurs non informatives à écarter de la colonne "nom"
VALEURS_A_EXCLURE <- c("RAS", "INCONNU", "Inconnu", "inconnu",
                       "NC", "nc", "Autre", "autre", "")

df_clean <- df %>%
  filter(
    fk_arb_etat == "EN PLACE",                          # uniquement arbres vivants
    !is.na(nom),                                         # pas de NA sur l'essence
    !(trimws(nom) %in% VALEURS_A_EXCLURE),               # pas de RAS/INCONNU/vide
    !is.na(clc_quartier),                                # quartier renseigné
    !(trimws(clc_quartier) %in% c("", "nan", "NA"))      # quartier non vide
  ) %>%
  mutate(
    nom          = trimws(nom),            # suppression espaces en bordure
    clc_quartier = trimws(clc_quartier)
  )

cat("✅ Après nettoyage :", nrow(df_clean), "arbres exploitables\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 5. IDENTIFICATION DU TOP 10 DES QUARTIERS LES PLUS PEUPLÉS
# ══════════════════════════════════════════════════════════════════════════════

top_quartiers <- df_clean %>%
  count(clc_quartier, sort = TRUE) %>%   # tri décroissant par nombre
  slice_head(n = 10) %>%                  # on garde les 10 premiers
  pull(clc_quartier)                      # extraction du vecteur de noms

cat("── Top 10 des quartiers les plus peuplés en arbres ──\n")
print(
  df_clean %>%
    filter(clc_quartier %in% top_quartiers) %>%
    count(clc_quartier, sort = TRUE)
)

# ══════════════════════════════════════════════════════════════════════════════
# 6. IDENTIFICATION DU TOP 10 DES ESSENCES LES PLUS FRÉQUENTES
# ══════════════════════════════════════════════════════════════════════════════

top_essences <- df_clean %>%
  count(nom, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(nom)

cat("\n── Top 10 des essences les plus fréquentes ──\n")
print(
  df_clean %>%
    filter(nom %in% top_essences) %>%
    count(nom, sort = TRUE)
)

# ══════════════════════════════════════════════════════════════════════════════
# 7. TABLEAU CROISÉ : QUARTIER × ESSENCE
# ══════════════════════════════════════════════════════════════════════════════
# On calcule pour chaque combinaison :
#   - nb_arbres  : nombre brut d'arbres
#   - pourcentage : part de l'essence dans le quartier
# ══════════════════════════════════════════════════════════════════════════════

df_croise <- df_clean %>%
  filter(
    clc_quartier %in% top_quartiers,
    nom          %in% top_essences
  ) %>%
  group_by(clc_quartier, nom) %>%
  summarise(nb_arbres = n(), .groups = "drop") %>%
  # Calcul du pourcentage au sein de chaque quartier
  group_by(clc_quartier) %>%
  mutate(pourcentage = round(nb_arbres / sum(nb_arbres) * 100, 1)) %>%
  ungroup()

cat("\n── Tableau croisé (", nrow(df_croise), "combinaisons) ──\n")
print(head(df_croise, 15))

# ══════════════════════════════════════════════════════════════════════════════
# 8. GRAPHIQUE 1 – BARRES EMPILÉES (PROPORTION D'ESSENCES PAR QUARTIER)
# ══════════════════════════════════════════════════════════════════════════════
# Chaque barre = 100 %, les couleurs montrent la composition interne du quartier
# Lecture : quel est le « profil végétal » de chaque quartier ?
# ══════════════════════════════════════════════════════════════════════════════

graph_barres <- ggplot(
  df_croise,
  aes(
    x    = reorder(clc_quartier, -nb_arbres, sum),   # tri par volume total
    y    = nb_arbres,
    fill = nom
  )
) +
  geom_bar(
    stat      = "identity",
    position  = "fill",            # toutes les barres montent à 100 %
    color     = "white",
    linewidth = 0.2
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  labs(
    title    = "Identité végétale des quartiers de Saint-Quentin",
    subtitle = "Répartition des 10 essences principales dans les 10 plus grands quartiers",
    x        = NULL,
    y        = "Proportion d'arbres",
    fill     = "Essence"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "grey40")
  )

print(graph_barres)

ggsave("essences_quartiers_barres.png", plot = graph_barres,
       width = 12, height = 6, dpi = 150)
cat("\n📊 Graphique sauvegardé : essences_quartiers_barres.png\n")

# ══════════════════════════════════════════════════════════════════════════════
# 9. GRAPHIQUE 2 – HEATMAP (CARTE DE CHALEUR)
# ══════════════════════════════════════════════════════════════════════════════
# Chaque case = une combinaison quartier × essence
# Couleur foncée = forte présence de cette essence dans ce quartier
# Lecture : détecter les patterns (essences dominantes, spécificités locales)
# ══════════════════════════════════════════════════════════════════════════════

graph_heatmap <- ggplot(
  df_croise,
  aes(
    x    = nom,
    y    = reorder(clc_quartier, nb_arbres, sum),
    fill = nb_arbres
  )
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = nb_arbres),
            size = 3.2, color = "grey20") +
  scale_fill_gradient(
    low  = "#f7fcf5",
    high = "#00441b",
    name = "Nb d'arbres"
  ) +
  labs(
    title    = "Heatmap : présence des essences par quartier",
    subtitle = "Top 10 quartiers × Top 10 essences",
    x        = "Essence",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid    = element_blank()
  )

print(graph_heatmap)

ggsave("essences_quartiers_heatmap.png", plot = graph_heatmap,
       width = 11, height = 7, dpi = 150)
cat("📊 Graphique sauvegardé : essences_quartiers_heatmap.png\n")

# ══════════════════════════════════════════════════════════════════════════════
# 10. SYNTHÈSE COMPLÉMENTAIRE – ESSENCE DOMINANTE PAR QUARTIER
# ══════════════════════════════════════════════════════════════════════════════
# Petit tableau texte qui identifie pour chaque quartier son essence majoritaire
# et sa proportion, exploitable directement dans le rapport.
# ══════════════════════════════════════════════════════════════════════════════

essence_dominante <- df_croise %>%
  group_by(clc_quartier) %>%
  slice_max(order_by = nb_arbres, n = 1) %>%
  select(clc_quartier, essence_dominante = nom, nb_arbres, pourcentage) %>%
  arrange(desc(nb_arbres))

cat("\n── Essence dominante par quartier ──\n")
print(essence_dominante)

# ══════════════════════════════════════════════════════════════════════════════
# 11. SYNTHÈSE FINALE
# ══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ✅ ANALYSE TERMINÉE\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Graphiques générés :\n")
cat("  • essences_quartiers_barres.png    (barres empilées 100 %)\n")
cat("  • essences_quartiers_heatmap.png   (carte de chaleur)\n")
cat("\nDossier de travail :", getwd(), "\n")