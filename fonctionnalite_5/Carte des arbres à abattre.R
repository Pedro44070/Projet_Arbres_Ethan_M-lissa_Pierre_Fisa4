#  CARTE DES ARBRES À ABATTRE à Saint-Quentin
#  Trinôme 4 : Pierre SICOT · Ethan HEURTIN · Mélissa BOUANCHAUD
#  ISEN Nantes – FISA4 – 2026

if (!require(dplyr))          install.packages("dplyr")
if (!require(leaflet))        install.packages("leaflet")
if (!require(leaflet.extras)) install.packages("leaflet.extras")
if (!require(htmlwidgets))    install.packages("htmlwidgets")

library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

df <- read.csv(
  file             = "C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv",     # → Patrimoine_Arbore_Nettoye.csv
  sep              = ";",
  header           = TRUE,
  encoding         = "UTF-8",
  stringsAsFactors = FALSE
)

# On garde uniquement les arbres à abattre avec coordonnées valides
df_abattre <- df %>%
  filter(
    fk_arb_etat %in% c("ABATTU", "SUPPRIMÉ", "Essouché"),
    !is.na(lon), !is.na(lat),
    lon != 0, lat != 0
  )

cat("arbres à abattre localisés :", nrow(df_abattre), "\n")
cat("   ABATTU    :", sum(df_abattre$fk_arb_etat == "ABATTU"), "\n")
cat("   SUPPRIMÉ  :", sum(df_abattre$fk_arb_etat == "SUPPRIMÉ"), "\n")
cat("   Essouché  :", sum(df_abattre$fk_arb_etat == "Essouché"), "\n\n")

# 3. CRÉATION DE L'ICÔNE "CROIX" POUR LES ARBRES À ABATTRE
# création "X" par zone
# pour avoir un rendu net à n'importe quel niveau de zoom

icone_croix <- makeIcon(
  iconUrl = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'><path d='M18.3 5.71L12 12l6.3 6.29-1.41 1.42L10.59 13.4 4.3 19.71 2.89 18.29 9.17 12 2.89 5.71 4.3 4.29l6.29 6.3 6.3-6.3z' fill='%23b80000' stroke='white' stroke-width='1'/></svg>",
  iconWidth    = 14,
  iconHeight   = 14,
  iconAnchorX  = 7,
  iconAnchorY  = 7
)

# COULEURS PAR TYPE D'ÉTAT

palette_etat <- colorFactor(
  palette = c("#c0392b", "#e67e22", "#8e44ad"),
  domain  = c("ABATTU", "Essouché", "SUPPRIMÉ")
)

# CARTE COMPLÈTE : HEATMAP + CROIX + LÉGENDE


gradient_chaleur <- c(
  "0.2" = "#2c7bb6",
  "0.4" = "#abd9e9",
  "0.6" = "#ffffbf",
  "0.8" = "#fdae61",
  "1.0" = "#d7191c"
)

carte_abattage <- leaflet(df_abattre) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(
    lng  = mean(df_abattre$lon),
    lat  = mean(df_abattre$lat),
    zoom = 13
  ) %>%
  
  # COUCHE 1 : densité
  addHeatmap(
    lng       = ~lon,
    lat       = ~lat,
    intensity = 1,
    radius    = 18,
    blur      = 25,
    max       = 0.05,
    gradient  = gradient_chaleur,
    group     = "Zones de concentration"
  ) %>%
  
  # COUCHE 2 : Croix précises à chaque position
  addMarkers(
    lng   = ~lon,
    lat   = ~lat,
    icon  = icone_croix,
    label = ~paste0("✕ ", fk_arb_etat, " – ", nom),
    popup = ~paste0(
      "<b>❌ Arbre à abattre</b><br>",
      "<b>État :</b> ",      fk_arb_etat,  "<br>",
      "<b>Essence :</b> ",   nom,          "<br>",
      "<b>Quartier :</b> ",  clc_quartier, "<br>",
      "<b>Secteur :</b> ",   clc_secteur
    ),
    group = "Positions exactes"
  ) %>%
  
  # LÉGENDE DE LA HEATMAP
  addLegend(
    position = "bottomright",
    colors   = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
    labels   = c("Très faible", "Faible", "Moyenne", "Élevée", "Très élevée"),
    title    = "Concentration d'abattages",
    opacity  = 0.9
  ) %>%
  
  # LÉGENDE DES CROIX
  addControl(
    html = paste0(
      "<div style='background:white; padding:8px 12px; border-radius:4px;",
      " box-shadow:0 0 10px rgba(0,0,0,0.2); font-family:Arial; font-size:12px;'>",
      "<b>✕ Position d'un arbre à abattre</b><br>",
      "<small>Total : <b>", nrow(df_abattre), " arbres</b> concernés</small>",
      "</div>"
    ),
    position = "topright"
  ) %>%
  
  # CONTRÔLE POUR AFFICHER/MASQUER LES COUCHES
  addLayersControl(
    overlayGroups = c("Zones de concentration", "Positions exactes"),
    options       = layersControlOptions(collapsed = FALSE),
    position      = "topleft"
  ) %>%
  
  addScaleBar(position = "bottomleft")

carte_abattage

# Export HTML
htmlwidgets::saveWidget(
  carte_abattage,
  "carte_arbres_abattre.html",
  selfcontained = TRUE
)

cat("📊 Carte sauvegardée : carte_arbres_abattre.html\n")
cat("Dossier :", getwd(), "\n")