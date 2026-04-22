# ══════════════════════════════════════════════════════════════════════════════
#  PROJET BIG DATA – PARTIE PRÉDICTIONS
#  Trinôme 4 : Pierre SICOT · Ethan HEURTIN · Mélissa BOUANCHAUD
#  ISEN Nantes – FISA4 – 2026
#
#  Objectifs :
#   1. Prédire l'âge estimé d'un arbre selon ses caractéristiques
#      (régression linéaire multiple)
#   2. Identifier les arbres susceptibles d'être abattus
#      (arbre de décision)
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Installation des packages (à exécuter une seule fois si besoin) ────────
# install.packages(c("dplyr", "lubridate", "ggplot2",
#                    "rpart", "rpart.plot"))

# ── 1. Chargement des packages ────────────────────────────────────────────────
library(dplyr)
library(lubridate)
library(ggplot2)
library(rpart)
library(rpart.plot)

# ── 2. Import du fichier CSV nettoyé ──────────────────────────────────────────
# ⚠️ Adapte le chemin ci-dessous à ton poste
df <- read.csv(
  file             = "C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv",
  sep              = ";",
  header           = TRUE,
  encoding         = "UTF-8",
  stringsAsFactors = FALSE
)

cat("✅ Fichier chargé :", nrow(df), "lignes,", ncol(df), "colonnes\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 3. PRÉPARATION DES DONNÉES
# ══════════════════════════════════════════════════════════════════════════════

df_model <- df %>%
  mutate(
    # Année de recensement extraite de la date
    annee_recens = year(ymd_hms(created_date, quiet = TRUE)),
    
    # Conversion des variables numériques
    age_estim    = as.numeric(age_estim),
    tronc_diam   = as.numeric(tronc_diam),
    haut_tot     = as.numeric(haut_tot),
    
    # Harmonisation des variables catégorielles
    remarquable  = tolower(trimws(remarquable)),
    fk_stadedev  = tolower(trimws(fk_stadedev)),
    fk_situation = as.factor(fk_situation),
    feuillage    = as.factor(feuillage),
    
    # Cible binaire pour la classification :
    # 1 = arbre à abattre ou déjà abattu, 0 = en place
    a_abattre = ifelse(
      fk_arb_etat %in% c("ABATTU", "SUPPRIMÉ", "Essouché"),
      1, 0
    )
  ) %>%
  # Suppression des valeurs aberrantes ou manquantes
  filter(
    !is.na(age_estim),   age_estim  > 0, age_estim  < 500,
    !is.na(tronc_diam),  tronc_diam > 0, tronc_diam < 500,
    !is.na(haut_tot),    haut_tot   > 0, haut_tot   < 50,
    !is.na(annee_recens)
  )

cat("✅ Lignes exploitables pour la modélisation :", nrow(df_model), "\n")
cat("   Arbres à abattre :", sum(df_model$a_abattre),
    "(", round(mean(df_model$a_abattre) * 100, 1), "%)\n\n")

# ── Séparation train / test (80 % / 20 %) ─────────────────────────────────────
set.seed(42)  # pour des résultats reproductibles
idx   <- sample(seq_len(nrow(df_model)), size = 0.8 * nrow(df_model))
train <- df_model[idx, ]
test  <- df_model[-idx, ]

cat("   Jeu d'entraînement :", nrow(train), "arbres\n")
cat("   Jeu de test        :", nrow(test),  "arbres\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. MODÈLE 1 – RÉGRESSION LINÉAIRE : PRÉDIRE L'ÂGE ESTIMÉ
# ══════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════\n")
cat("  MODÈLE 1 – Régression linéaire multiple sur l'âge estimé\n")
cat("══════════════════════════════════════════════════════════════\n\n")

modele_age <- lm(
  age_estim ~ annee_recens + tronc_diam + haut_tot + remarquable + fk_situation,
  data = train
)

# Résumé statistique du modèle
print(summary(modele_age))

# Prédictions sur le jeu de test
pred_age <- predict(modele_age, newdata = test)

# Métriques d'évaluation
rmse <- sqrt(mean((test$age_estim - pred_age)^2, na.rm = TRUE))
mae  <- mean(abs(test$age_estim - pred_age), na.rm = TRUE)
r2   <- cor(pred_age, test$age_estim, use = "complete.obs")^2

cat("\n── Performance sur le jeu de test ──\n")
cat("RMSE (erreur quadratique moyenne) :", round(rmse, 2), "ans\n")
cat("MAE  (écart absolu moyen)         :", round(mae, 2),  "ans\n")
cat("R²   (qualité de l'ajustement)    :", round(r2, 3),   "\n\n")

# ── Visualisation : prédit vs réel ────────────────────────────────────────────
graph_age <- ggplot(
  data.frame(reel = test$age_estim, predit = pred_age),
  aes(x = reel, y = predit)
) +
  geom_point(alpha = 0.35, color = "#2E75B6") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title    = "Prédiction de l'âge estimé : prédit vs réel",
    subtitle = paste0("RMSE = ", round(rmse, 1), " ans  ·  R² = ", round(r2, 3)),
    x        = "Âge réel (années)",
    y        = "Âge prédit (années)"
  ) +
  theme_minimal(base_size = 12)

print(graph_age)
ggsave("regression_age.png", plot = graph_age,
       width = 8, height = 6, dpi = 150)
cat("📊 Graphique sauvegardé : regression_age.png\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 5. MODÈLE 2 – ARBRE DE DÉCISION : PRÉDIRE LES ABATTAGES
# ══════════════════════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════════════\n")
cat("  MODÈLE 2 – Arbre de décision pour identifier les abattages\n")
cat("══════════════════════════════════════════════════════════════\n\n")

modele_abattre <- rpart(
  as.factor(a_abattre) ~ age_estim + tronc_diam + haut_tot +
    fk_stadedev + fk_situation + remarquable,
  data    = train,
  method  = "class",
  control = rpart.control(cp = 0.005, maxdepth = 5)
)

# ── Visualisation de l'arbre de décision ──────────────────────────────────────
rpart.plot(
  modele_abattre,
  type        = 4,
  extra       = 104,
  box.palette = "RdGn",
  main        = "Arbre de décision – Arbres à abattre"
)

# Sauvegarde en PNG pour le rapport
png("arbre_decision_abattage.png", width = 1400, height = 900, res = 130)
rpart.plot(
  modele_abattre,
  type        = 4,
  extra       = 104,
  box.palette = "RdGn",
  main        = "Arbre de décision – Arbres à abattre"
)
dev.off()
cat("📊 Graphique sauvegardé : arbre_decision_abattage.png\n\n")

# ── Évaluation sur le jeu de test ─────────────────────────────────────────────
pred_classe  <- predict(modele_abattre, newdata = test, type = "class")
matrice_conf <- table(Réel = test$a_abattre, Prédit = pred_classe)

cat("── Matrice de confusion ──\n")
print(matrice_conf)

accuracy <- sum(diag(matrice_conf)) / sum(matrice_conf)

# Précision et rappel pour la classe "à abattre" (1)
vrais_positifs   <- matrice_conf["1", "1"]
faux_positifs    <- matrice_conf["0", "1"]
faux_negatifs    <- matrice_conf["1", "0"]
precision_pos    <- vrais_positifs / (vrais_positifs + faux_positifs)
rappel_pos       <- vrais_positifs / (vrais_positifs + faux_negatifs)

cat("\n── Performance globale ──\n")
cat("Précision globale (accuracy) :", round(accuracy * 100, 1),  "%\n")
cat("Précision classe 'à abattre' :", round(precision_pos * 100, 1), "%\n")
cat("Rappel    classe 'à abattre' :", round(rappel_pos    * 100, 1), "%\n\n")

# ── Importance des variables ──────────────────────────────────────────────────
cat("── Variables les plus importantes pour la décision ──\n")
importance <- sort(modele_abattre$variable.importance, decreasing = TRUE)
print(round(importance, 1))

# ══════════════════════════════════════════════════════════════════════════════
# 6. SYNTHÈSE
# ══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ✅ ANALYSE TERMINÉE\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Graphiques générés dans le dossier de travail :\n")
cat("  • regression_age.png          (modèle 1)\n")
cat("  • arbre_decision_abattage.png (modèle 2)\n")
cat("\nDossier de travail courant :", getwd(), "\n")