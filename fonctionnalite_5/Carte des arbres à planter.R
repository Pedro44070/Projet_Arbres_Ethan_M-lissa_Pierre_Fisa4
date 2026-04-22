library(leaflet)
library(dplyr)
library(MASS)
library(raster)

#  Importation des données
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", 
               sep = ";", stringsAsFactors = FALSE)
# 1. Préparation : On ne garde que les arbres vivants
df_vivants <- df %>% filter(fk_arb_etat == "EN PLACE")

# 2. Calcul de la densité (KDE)
# n = 200 (résolution), h = lissage de la courbe
densite <- kde2d(df_vivants$lon, df_vivants$lat, n = 200, h = 0.008)

# 3. Conversion en Raster (image géographique)
r <- raster(list(x = densite$x, y = densite$y, z = densite$z))
crs(r) <- "+proj=longlat +datum=WGS84"

# 4. Calcul des seuils de priorité (Quantiles)
vals <- values(r)[values(r) > quantile(values(r), 0.1, na.rm=T)] # On ignore le vide
seuils <- quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# 5. Création de la palette de couleurs "Priorité"
pal <- colorBin(
  palette = c("#c0392b", "#e67e22", "#f1c40f", "#27ae60"), # Rouge à Vert
  bins = c(0, seuils, max(vals, na.rm=T)),
  na.color = "transparent"
)

# 6. Carte interactive
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r, colors = pal, opacity = 0.6) %>%
  addLegend(
    colors = c("#c0392b", "#e67e22", "#f1c40f", "#27ae60"),
    labels = c("Rouge : Zone vide", "Orange : Faible", "Jaune : Moyen", "Vert : Dense"),
    title = "Plan de Plantation"
  )