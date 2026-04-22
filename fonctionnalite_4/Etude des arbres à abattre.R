# ══════════════════════════════════════════════════════════════════════════════
#  PROFIL COMPARATIF – Arbres abattus vs arbres sains
#  Trinôme 4 : Pierre SICOT · Ethan HEURTIN · Mélissa BOUANCHAUD
#  ISEN Nantes – FISA4 – 2026
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Packages ───────────────────────────────────────────────────────────────
if (!require(dplyr))   install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(scales))  install.packages("scales")

library(dplyr)
library(ggplot2)
library(scales)

# ══════════════════════════════════════════════════════════════════════════════
# 2. IMPORT ET PRÉPARATION
# ══════════════════════════════════════════════════════════════════════════════

df <- read.csv(
  file             = "C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv",   # → Patrimoine_Arbore_Nettoye.csv
  sep              = ";",
  header           = TRUE,
  encoding         = "UTF-8",
  stringsAsFactors = FALSE
)

# Création de la variable de groupe + nettoyage minimal
df_clean <- df %>%
  mutate(
    age_estim   = as.numeric(gsub(",", ".", as.character(age_estim))),
    tronc_diam  = as.numeric(gsub(",", ".", as.character(tronc_diam))),
    fk_stadedev = tolower(trimws(fk_stadedev)),
    
    # Groupe : Abattu / Sain / exclus
    groupe = case_when(
      fk_arb_etat %in% c("ABATTU", "SUPPRIMÉ", "Essouché") ~ "Abattu",
      fk_arb_etat == "EN PLACE"                            ~ "Sain",
      TRUE                                                  ~ NA_character_
    )
  ) %>%
  filter(!is.na(groupe))

cat("✅ Données préparées :", nrow(df_clean), "arbres\n")
cat("   Sains   :", sum(df_clean$groupe == "Sain"), "\n")
cat("   Abattus :", sum(df_clean$groupe == "Abattu"), "\n\n")

# Palette commune : vert = sain, rouge = abattu
couleurs <- c("Sain" = "#2ecc71", "Abattu" = "#e74c3c")

# ══════════════════════════════════════════════════════════════════════════════
# 3. GRAPHIQUE 1 – ÂGE ESTIMÉ (BOXPLOT)
# ══════════════════════════════════════════════════════════════════════════════

df_age <- df_clean %>%
  filter(!is.na(age_estim), age_estim >= 1, age_estim <= 200)

graph_age <- ggplot(df_age, aes(x = groupe, y = age_estim, fill = groupe)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2, width = 0.5) +
  scale_fill_manual(values = couleurs) +
  labs(
    title = "Âge estimé : abattus vs sains",
    x     = NULL,
    y     = "Âge estimé (années)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold"))

print(graph_age)
ggsave("comparaison_age.png", graph_age, width = 7, height = 5, dpi = 150)

# ══════════════════════════════════════════════════════════════════════════════
# 4. GRAPHIQUE 2 – DIAMÈTRE DU TRONC (BOXPLOT)
# ══════════════════════════════════════════════════════════════════════════════

df_diam <- df_clean %>%
  filter(!is.na(tronc_diam), tronc_diam > 0, tronc_diam < 500)

graph_diam <- ggplot(df_diam, aes(x = groupe, y = tronc_diam, fill = groupe)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2, width = 0.5) +
  scale_fill_manual(values = couleurs) +
  labs(
    title = "Diamètre du tronc : abattus vs sains",
    x     = NULL,
    y     = "Diamètre du tronc (cm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold"))

print(graph_diam)
ggsave("comparaison_diametre.png", graph_diam, width = 7, height = 5, dpi = 150)

# ══════════════════════════════════════════════════════════════════════════════
# 5. GRAPHIQUE 3 – STADE DE DÉVELOPPEMENT (BARRES 100 %)
# ══════════════════════════════════════════════════════════════════════════════

stades_valides <- c("jeune", "adulte", "vieux", "senescent")

df_stade <- df_clean %>%
  filter(fk_stadedev %in% stades_valides) %>%
  mutate(fk_stadedev = factor(fk_stadedev, levels = stades_valides))

graph_stade <- ggplot(df_stade, aes(x = fk_stadedev, fill = groupe)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = couleurs, name = "Statut") +
  labs(
    title = "Proportion d'arbres abattus par stade",
    x     = "Stade de développement",
    y     = "Proportion"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(graph_stade)
ggsave("comparaison_stade.png", graph_stade, width = 8, height = 5, dpi = 150)

# ══════════════════════════════════════════════════════════════════════════════
# 6. SYNTHÈSE CHIFFRÉE
# ══════════════════════════════════════════════════════════════════════════════

cat("── Moyennes par groupe ──\n")
synthese <- df_clean %>%
  group_by(groupe) %>%
  summarise(
    n             = n(),
    age_moyen     = round(mean(age_estim,  na.rm = TRUE), 1),
    age_median    = median(age_estim,  na.rm = TRUE),
    diam_moyen    = round(mean(tronc_diam, na.rm = TRUE), 1),
    diam_median   = median(tronc_diam, na.rm = TRUE),
    .groups       = "drop"
  )
print(synthese)

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ✅ ANALYSE TERMINÉE\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Graphiques sauvegardés :\n")
cat("  • comparaison_age.png        (âge estimé)\n")
cat("  • comparaison_diametre.png   (diamètre du tronc)\n")
cat("  • comparaison_stade.png      (stade de développement)\n")
cat("\nDossier :", getwd(), "\n")