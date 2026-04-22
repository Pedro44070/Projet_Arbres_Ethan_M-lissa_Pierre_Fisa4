library(readxl)
library(dplyr)

# ── 1. Chargement ──────────────────────────────────────────────────────────────
df <- read_excel("C:/Users/PC/Downloads/Data.xlsx")

# ── 2. Nettoyage ───────────────────────────────────────────────────────────────
df_clean <- df %>%
  mutate(
    fk_stadedev = tolower(trimws(fk_stadedev)),
    age_estim   = as.numeric(age_estim)
  ) %>%
  filter(fk_stadedev %in% c("jeune", "adulte", "vieux", "senescent")) %>%
  filter(!is.na(age_estim), age_estim > 0, age_estim < 500)

cat("Lignes conservées après nettoyage :", nrow(df_clean), "\n")

# ── 3. Statistiques descriptives ───────────────────────────────────────────────
stats <- df_clean %>%
  group_by(fk_stadedev) %>%
  summarise(
    nb_arbres  = n(),
    age_min    = min(age_estim),
    age_moyen  = round(mean(age_estim), 1),
    age_median = median(age_estim),
    age_max    = max(age_estim),
    ecart_type = round(sd(age_estim), 1),
    q1         = quantile(age_estim, 0.25),
    q3         = quantile(age_estim, 0.75)
  ) %>%
  arrange(age_moyen)

print(stats)

# ── 4. Test de Kruskal-Wallis ──────────────────────────────────────────────────
kruskal_result <- kruskal.test(age_estim ~ fk_stadedev, data = df_clean)
cat("\n── Test de Kruskal-Wallis ──\n")
print(kruskal_result)
if (kruskal_result$p.value < 0.05) {
  cat("✅ Les catégories ont des âges significativement différents (p <",
      round(kruskal_result$p.value, 4), ")\n")
} else {
  cat("⚠️ Pas de différence significative\n")
}

# ── 5. Boxplot ─────────────────────────────────────────────────────────────────
df_clean$fk_stadedev <- factor(
  df_clean$fk_stadedev,
  levels = c("jeune", "adulte", "vieux", "senescent")
)

boxplot(
  age_estim ~ fk_stadedev,
  data  = df_clean,
  col   = c("#a8d5a2", "#f4c542", "#e07b39", "#c0392b"),
  main  = "Distribution de l'âge par stade de développement",
  xlab  = "Stade de développement",
  ylab  = "Âge estimé (années)"
)
means <- tapply(df_clean$age_estim, df_clean$fk_stadedev, mean)
points(1:4, means, pch = 18, col = "red", cex = 1.5)
legend("topleft", legend = "Moyenne", pch = 18, col = "red")

dev.copy(png, "boxplot_age_stade.png", width = 800, height = 600)
dev.off()
cat("\n📊 Graphique sauvegardé : boxplot_age_stade.png\n")