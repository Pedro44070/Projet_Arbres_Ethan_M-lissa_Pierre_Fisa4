# 1. Installation des packages si besoin
if(!require(leaflet)) install.packages("leaflet")
if(!require(dplyr)) install.packages("dplyr")
if(!require(grDevices)) install.packages("grDevices")

library(leaflet)
library(dplyr)

# 2. Chargement des données
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", 
               sep = ";", stringsAsFactors = FALSE)

df_map <- df %>%
  mutate(clc_quartier = trimws(clc_quartier)) %>%
  filter(!is.na(lon) & !is.na(lat), lon != 0, lat != 0)

# 3. CRÉATION D'UNE PALETTE À TRÈS HAUT CONTRASTE
# On récupère la liste unique des quartiers
quartiers_uniques <- unique(df_map$clc_quartier)
n <- length(quartiers_uniques)

# ASTUCE : On génère des couleurs sur tout le spectre et on les MÉLANGE (sample)
# pour éviter que des quartiers proches alphabétiquement aient des couleurs proches.
set.seed(42) # Pour garder les mêmes couleurs à chaque lancement
couleurs_melangees <- sample(rainbow(n, s = 0.8, v = 0.9))

ma_palette <- colorFactor(palette = couleurs_melangees, domain = quartiers_uniques)

# 4. Construction de la carte
leaflet(df_map) %>%
  # Fond de carte sombre (DarkMatter) : fait ressortir les couleurs vives
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    color = "white",       # Contour blanc fin pour détacher le point du fond noir
    weight = 0.5,
    fillColor = ~ma_palette(clc_quartier), 
    radius = 5,
    fillOpacity = 0.8,
    stroke = TRUE,
    label = ~as.character(clc_quartier),
    popup = ~paste("<b>Quartier :</b>", clc_quartier, "<br><b>Essence :</b>", nom)
  ) %>%
  
  addLegend(
    position = "bottomright",
    pal = ma_palette,
    values = ~clc_quartier,
    title = "Quartiers distincts",
    opacity = 1
  )