# ══════════════════════════════════════════════════════════════════════════════
#  PROJET BIG DATA – PARTIE CARTOGRAPHIE & POLITIQUE URBAINE
#  Trinôme 4 : Pierre SICOT · Ethan HEURTIN · Mélissa BOUANCHAUD
#  ISEN Nantes – FISA4 – 2026
#
#  Objectifs :
#   1. Visualiser le patrimoine arboré sur une carte interactive par année
#   2. Identifier les zones où planter de nouveaux arbres pour harmoniser
#      la densité végétale de Saint-Quentin (méthode KDE + seuils par quantiles)
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Installation des packages (uniquement si non déjà installés) ───────────
if (!require(leaflet))     install.packages("leaflet")
if (!require(htmlwidgets)) install.packages("htmlwidgets")
if (!require(lubridate))   install.packages("lubridate")
if (!require(dplyr))       install.packages("dplyr")
if (!require(ggplot2))     install.packages("ggplot2")
if (!require(ggridges))    install.packages("ggridges")
if (!require(corrplot))    install.packages("corrplot")
if (!require(MASS))        install.packages("MASS")
if (!require(raster))      install.packages("raster")

# ── 2. Chargement des librairies ──────────────────────────────────────────────
library(leaflet)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggridges)
library(corrplot)
library(htmlwidgets)
library(MASS)
library(raster)

# Forcer dplyr::select pour éviter le conflit avec raster::select
select <- dplyr::select

# ── 3. Chargement du fichier CSV ──────────────────────────────────────────────
df <- read.csv(
  "C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv",
  sep              = ";",
  stringsAsFactors = FALSE,
  encoding         = "UTF-8"
)

cat("✅ Fichier chargé :", nrow(df), "lignes,", ncol(df), "colonnes\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 4. PRÉPARATION DES DONNÉES POUR LA CARTOGRAPHIE
# ══════════════════════════════════════════════════════════════════════════════

df_map <- df %>%
  mutate(
    age_estim  = as.numeric(gsub(",", ".", as.character(age_estim))),
    tronc_diam = as.numeric(gsub(",", ".", as.character(tronc_diam))) * 100,
    haut_tot   = as.numeric(gsub(",", ".", as.character(haut_tot))),
    
    created_date = as.POSIXct(created_date, format = "%Y-%m-%d %H:%M:%S"),
    annee        = year(created_date),
    
    remarquable_clean = tolower(trimws(remarquable)),
    
    couleur = case_when(
      remarquable_clean %in% c("oui", "1", "true") ~ "#f1c40f",
      fk_arb_etat == "EN PLACE"                    ~ "#2ecc71",
      TRUE                                          ~ "#e74c3c"
    ),
    
    taille = ifelse(remarquable_clean %in% c("oui", "1", "true"), 7, 4)
  ) %>%
  filter(!is.na(lon) & !is.na(lat), lon != 0, lat != 0)

cat("✅ Données préparées :", nrow(df_map), "arbres exploitables\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 5. CARTOGRAPHIE – VUE D'ENSEMBLE DU PATRIMOINE PAR ANNÉE
# ══════════════════════════════════════════════════════════════════════════════

generer_carte_fond <- function(data_a_afficher, titre_legende) {
  
  if (nrow(data_a_afficher) == 0) {
    return("Aucune donnée à afficher pour cette sélection")
  }
  
  leaflet(data_a_afficher) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng         = ~lon,
      lat         = ~lat,
      color       = "white",
      weight      = 1,
      fillColor   = ~couleur,
      radius      = ~taille,
      fillOpacity = 0.9,
      stroke      = TRUE,
      popup       = ~paste(
        "<b>Essence :</b>",       nom,                             "<br>",
        "<b>État :</b>",          fk_arb_etat,                     "<br>",
        "<b>Remarquable :</b>",   remarquable_clean,               "<br>",
        "<b>Quartier :</b>",      clc_quartier,                    "<br>",
        "<b>Date création :</b>", format(created_date, "%d/%m/%Y")
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors   = c("#f1c40f", "#2ecc71", "#e74c3c"),
      labels   = c("Arbres remarquables", "En place", "Supprimé/Essouché"),
      title    = titre_legende,
      opacity  = 1
    )
}

carte_2017   <- generer_carte_fond(df_map %>% filter(annee == 2017), "Arbres créés en 2017")
carte_2018   <- generer_carte_fond(df_map %>% filter(annee == 2018), "Arbres créés en 2018")
carte_2019   <- generer_carte_fond(df_map %>% filter(annee == 2019), "Arbres créés en 2019")
carte_2020   <- generer_carte_fond(df_map %>% filter(annee == 2020), "Arbres créés en 2020")
carte_totale <- generer_carte_fond(df_map, "Totalité du Patrimoine")

carte_totale

# ══════════════════════════════════════════════════════════════════════════════
# 6. ANALYSE – OÙ PLANTER DE NOUVEAUX ARBRES ? (VERSION CALIBRÉE)
# Méthode : KDE + seuils par quantiles + masquage des zones hors-ville
# ══════════════════════════════════════════════════════════════════════════════

# ── 6.1 Arbres vivants uniquement ─────────────────────────────────────────────
df_vivants <- df_map %>%
  filter(fk_arb_etat == "EN PLACE")

cat("── Analyse des zones de plantation (KDE calibré) ──\n")
cat("Arbres vivants analysés :", nrow(df_vivants), "\n")

# ── 6.2 Emprise géographique serrée autour de la zone urbaine ─────────────────
# On utilise des quantiles 2-98% pour exclure les arbres isolés qui élargiraient
# artificiellement l'emprise. Cela concentre la carte sur la ville elle-même.


lng_min <- min(df_vivants$lon)
lng_max <- max(df_vivants$lon)
lat_min <- min(df_vivants$lat)
lat_max <- max(df_vivants$lat)

# Petite marge autour pour ne pas couper sec
marge_lng <- (lng_max - lng_min) * 0.03
marge_lat <- (lat_max - lat_min) * 0.03
lng_min <- lng_min - marge_lng
lng_max <- lng_max + marge_lng
lat_min <- lat_min - marge_lat
lat_max <- lat_max + marge_lat

# ── 6.3 Estimation KDE avec bande passante adaptée ────────────────────────────
# h plus petit = carte plus fine, qui colle mieux aux rues (pas aux quartiers entiers)
RESOLUTION  <- 300

densite_kde <- kde2d(
  x    = df_vivants$lon,
  y    = df_vivants$lat,
  n    = RESOLUTION,
  h    = c(0.008, 0.008),   # bande passante fixée pour éviter les gros halos
  lims = c(lng_min, lng_max, lat_min, lat_max)
)

cat("✅ KDE calculée : grille", RESOLUTION, "×", RESOLUTION, "pixels\n")

# ── 6.4 Conversion en raster ──────────────────────────────────────────────────
raster_densite <- raster(
  list(x = densite_kde$x, y = densite_kde$y, z = densite_kde$z)
)
crs(raster_densite) <- "+proj=longlat +datum=WGS84"

# ── 6.5 CALIBRATION PAR QUANTILES — ceci corrige le problème de lecture ───────
# On calcule les seuils sur la distribution RÉELLE de la densité urbaine
# plutôt que sur le min/max brut qui écrase tout vers le rouge
vals <- values(raster_densite)
vals <- vals[!is.na(vals)]

seuil_q25 <- quantile(vals, 0.25)   # en dessous = zone peu dense
seuil_q50 <- quantile(vals, 0.50)   # médiane
seuil_q75 <- quantile(vals, 0.75)   # au-dessus = zone bien dotée

cat("   Seuils de densité calibrés :\n")
cat("   - Q25 (priorité haute)  :", round(seuil_q25, 2), "\n")
cat("   - Q50 (médiane)         :", round(seuil_q50, 2), "\n")
cat("   - Q75 (bien dotée)      :", round(seuil_q75, 2), "\n")

# ── 6.6 MASQUAGE des zones très isolées (périphéries sans aucun arbre) ────────
# On met à NA les cellules en dessous d'un seuil très bas pour qu'elles
# deviennent transparentes sur la carte (pas colorées en rouge fallacieusement)
seuil_masque <- quantile(vals, 0.10)  # on masque le décile le plus faible
raster_filtre <- raster_densite
values(raster_filtre)[values(raster_filtre) < seuil_masque] <- NA

# ── 6.7 Palette DISCRÈTE à 4 classes basée sur les quantiles ──────────────────
# 4 couleurs distinctes correspondant aux 4 niveaux de priorité
palette_disc <- colorBin(
  palette = c("#c0392b", "#e67e22", "#f1c40f", "#27ae60"),  # rouge→orange→jaune→vert
  bins    = c(seuil_masque, seuil_q25, seuil_q50, seuil_q75, max(vals)),
  na.color = "transparent"
)

# ── 6.8 Construction de la carte ──────────────────────────────────────────────
carte_plantation <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(
    lng  = mean(df_vivants$lon),
    lat  = mean(df_vivants$lat),
    zoom = 13
  ) %>%
  
  addRasterImage(
    raster_filtre,
    colors  = palette_disc,
    opacity = 0.6,
    project = FALSE
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors   = c("#c0392b", "#e67e22", "#f1c40f", "#27ae60"),
    labels   = c("🔴 À planter en priorité",
                 "🟠 Densité faible",
                 "🟡 Densité moyenne",
                 "🟢 Bien dotée"),
    title    = "Politique de plantation",
    opacity  = 0.8
  ) %>%
  
  addCircleMarkers(
    data        = df_vivants %>% filter(remarquable_clean %in% c("oui", "1", "true")),
    lng         = ~lon,
    lat         = ~lat,
    color       = "#8e44ad",
    fillColor   = "#f1c40f",
    radius      = 4,
    weight      = 1,
    fillOpacity = 0.9,
    stroke      = TRUE,
    popup       = ~paste0("⭐ <b>", nom, "</b><br/>Quartier : ", clc_quartier),
    group       = "Arbres remarquables"
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Arbres remarquables"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addScaleBar(position = "bottomleft")

carte_plantation

htmlwidgets::saveWidget(
  carte_plantation,
  "carte_densite_plantation.html",
  selfcontained = TRUE
)
cat("\n📊 Carte sauvegardée : carte_densite_plantation.html\n")

# ══════════════════════════════════════════════════════════════════════════════
# 7. BONUS – ANALYSE COMPLÉMENTAIRE PAR QUARTIER ADMINISTRATIF
# ══════════════════════════════════════════════════════════════════════════════

analyse_quartiers <- df_vivants %>%
  filter(!is.na(clc_quartier), clc_quartier != "", clc_quartier != "nan") %>%
  group_by(clc_quartier) %>%
  summarise(
    nb_arbres = n(),
    .groups   = "drop"
  ) %>%
  arrange(nb_arbres) %>%
  mutate(
    rang = row_number(),
    recommandation = case_when(
      nb_arbres <  quantile(nb_arbres, 0.33) ~ "🔴 Prioriser plantations",
      nb_arbres <  quantile(nb_arbres, 0.66) ~ "🟡 Compléter si possible",
      TRUE                                    ~ "🟢 Bien pourvu"
    )
  )

cat("\n── Classement des quartiers (du moins doté au mieux doté) ──\n")
print(analyse_quartiers)

graph_quartiers <- ggplot(
  analyse_quartiers,
  aes(
    x    = reorder(clc_quartier, nb_arbres),
    y    = nb_arbres,
    fill = recommandation
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "🔴 Prioriser plantations" = "#c0392b",
    "🟡 Compléter si possible" = "#f1c40f",
    "🟢 Bien pourvu"           = "#27ae60"
  )) +
  labs(
    title = "Nombre d'arbres par quartier – Priorités de plantation",
    x     = NULL,
    y     = "Nombre d'arbres en place",
    fill  = "Recommandation"
  ) +
  theme_minimal(base_size = 11)

print(graph_quartiers)

ggsave("quartiers_plantation.png", plot = graph_quartiers,
       width = 10, height = 6, dpi = 150)
cat("📊 Graphique sauvegardé : quartiers_plantation.png\n")

# ══════════════════════════════════════════════════════════════════════════════
# 8. SYNTHÈSE
# ══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ✅ ANALYSE TERMINÉE\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("Livrables produits :\n")
cat("  • carte_densite_plantation.html   (KDE + quantiles + masquage)\n")
cat("  • quartiers_plantation.png        (classement des quartiers)\n")
cat("\nDossier de travail courant :", getwd(), "\n")