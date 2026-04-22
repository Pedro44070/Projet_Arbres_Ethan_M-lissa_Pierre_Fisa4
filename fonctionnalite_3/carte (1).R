# 1. Installation des packages (si besoin)
if(!require(leaflet)) install.packages("leaflet")
if(!require(htmlwidgets)) install.packages("htmlwidgets")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggridges)) install.packages("ggridges")
if(!require(corrplot)) install.packages("corrplot")

# 2. Chargement des librairies
library(leaflet)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggridges)
library(corrplot)

# 3. Chargement des données
# Note : Vérifie que le nom du fichier correspond bien à ton dossier Downloads
df <- read.csv("C:/Users/bouan/Downloads/Patrimoine_Arbore_Nettoye (4).csv", 
               sep = ";", stringsAsFactors = FALSE)

# 4. Préparation des données
df_map <- df %>%
  mutate(
    # Conversion numérique (nettoyage des virgules si présentes)
    age_estim = as.numeric(gsub(",", ".", as.character(age_estim))),
    tronc_diam = as.numeric(gsub(",", ".", as.character(tronc_diam))) * 100, # cm
    haut_tot = as.numeric(gsub(",", ".", as.character(haut_tot))),
    
    # Gestion des dates spécifique au fichier (format avec tirets)
    created_date = as.POSIXct(created_date, format="%Y-%m-%d %H:%M:%S"),
    annee = year(created_date),
    
    # Nettoyage colonne remarquable
    remarquable_clean = tolower(trimws(remarquable)),
    
    # Couleur des points
    couleur = case_when(
      remarquable_clean %in% c("oui", "1", "true") ~ "#f1c40f",  # Jaune ⭐
      fk_arb_etat == "EN PLACE" ~ "#2ecc71",                     # Vert
      TRUE ~ "#e74c3c"                                           # Rouge
    ),
    
    # Taille des points
    taille = ifelse(remarquable_clean %in% c("oui", "1", "true"), 7, 4)
  ) %>%
  # Filtrage des points avec coordonnées valides
  filter(!is.na(lon) & !is.na(lat), lon != 0, lat != 0)


# ==========================================================
# PARTIE 2 : CARTOGRAPHIE LEAFLET
# ==========================================================

# Fonction générique pour la carte
generer_carte_fond <- function(data_a_afficher, titre_legende) {
  
  if(nrow(data_a_afficher) == 0) return("Aucune donnée à afficher pour cette sélection")
  
  leaflet(data_a_afficher) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Centrage dynamique
    addCircleMarkers(
      lng = ~lon, lat = ~lat,
      color = "white",
      weight = 1,
      fillColor = ~couleur,
      radius = ~taille,
      fillOpacity = 0.9,
      stroke = TRUE,
      popup = ~paste(
        "<b>Essence :</b>", nom, "<br>",
        "<b>Etat :</b>", fk_arb_etat, "<br>",
        "<b>Remarquable :</b>", remarquable_clean, "<br>",
        "<b>Quartier :</b>", clc_quartier, "<br>",
        "<b>Date création :</b>", format(created_date, "%d/%m/%Y")
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#f1c40f", "#2ecc71", "#e74c3c"),
      labels = c("Arbres remarquables", "En place", "Supprimé/Essouché"),
      title = titre_legende,
      opacity = 1
    )
}

# 6. Génération des différentes vues
carte_2017 <- generer_carte_fond(df_map %>% filter(annee == 2017), "Arbres créés en 2017")
carte_2018 <- generer_carte_fond(df_map %>% filter(annee == 2018), "Arbres créés en 2018")
carte_2019 <- generer_carte_fond(df_map %>% filter(annee == 2019), "Arbres créés en 2019")
carte_2020 <- generer_carte_fond(df_map %>% filter(annee == 2020), "Arbres créés en 2020")
carte_totale <- generer_carte_fond(df_map, "Totalité du Patrimoine")

# 7. Affichage
# Exécute ces lignes une par une pour voir les résultats
carte_2017
carte_2018
carte_2019
carte_2020
carte_totale
