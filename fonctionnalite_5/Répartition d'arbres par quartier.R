# Projet Big Data - Trinôme 4
# Visualisation cartographique et analyse des quartiers

# 1. Chargement des librairies
library(leaflet)
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(raster)
library(htmlwidgets)

# On évite les conflits entre dplyr et raster
select <- dplyr::select

# 2. Lecture des données
df <- read.csv("C:/Users/PC/Downloads/Patrimoine_Arbore_Nettoye.csv", sep=";", stringsAsFactors=FALSE)

# 3. Préparation et nettoyage pour les cartes
df_map <- df %>%
  mutate(
    # On remplace les virgules et on passe en numérique
    age_estim = as.numeric(gsub(",", ".", age_estim)),
    tronc_diam = as.numeric(gsub(",", ".", tronc_diam)) * 100,
    haut_tot = as.numeric(gsub(",", ".", haut_tot)),
    
    # Extraction de l'année
    annee = year(as.POSIXct(created_date, format="%Y-%m-%d %H:%M:%S")),
    remarquable_clean = tolower(trimws(remarquable)),
    
    # Définition des couleurs pour Leaflet
    couleur = case_when(
      remarquable_clean %in% c("oui", "1", "true") ~ "#f1c40f", # Jaune (remarquable)
      fk_arb_etat == "EN PLACE" ~ "#2ecc71",                    # Vert (vivant)
      TRUE ~ "#e74c3c"                                          # Rouge (abattu)
    ),
    taille = ifelse(remarquable_clean %in% c("oui", "1", "true"), 7, 4)
  ) %>%
  # On vire les lignes sans coordonnées
  filter(!is.na(lon), !is.na(lat), lon != 0, lat != 0)


# 4. Fonction pour générer rapidement les cartes par année
creer_carte <- function(data, titre) {
  leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      ~lon, ~lat, color="white", weight=1, fillColor=~couleur,
      radius=~taille, fillOpacity=0.9, stroke=TRUE,
      popup=~paste("<b>Essence :</b>", nom, "<br><b>Etat :</b>", fk_arb_etat)
    ) %>%
    addLegend("bottomright", colors=c("#f1c40f", "#2ecc71", "#e74c3c"),
              labels=c("Remarquable", "En place", "Supprimé"), title=titre)
}

# Exemples de cartes
carte_2017 <- creer_carte(df_map %>% filter(annee == 2017), "Créés en 2017")
carte_totale <- creer_carte(df_map, "Tous les arbres")


# =========================================================
# 5. CARTE DE DENSITE (OÙ PLANTER ?) - KDE
# =========================================================

# On ne garde que les arbres vivants
df_vivants <- df_map %>% filter(fk_arb_etat == "EN PLACE")

# On définit la zone de la carte (avec une petite marge)
marge = 0.005
lng_min <- min(df_vivants$lon) - marge
lng_max <- max(df_vivants$lon) + marge
lat_min <- min(df_vivants$lat) - marge
lat_max <- max(df_vivants$lat) + marge

# Calcul de la densité (Heatmap lisse)
densite <- kde2d(df_vivants$lon, df_vivants$lat, n=300, h=c(0.008, 0.008), 
                 lims=c(lng_min, lng_max, lat_min, lat_max))

# Conversion en objet Raster pour l'afficher sur Leaflet
rast <- raster(list(x=densite$x, y=densite$y, z=densite$z))
crs(rast) <- "+proj=longlat +datum=WGS84"

# Calibration des couleurs avec les quantiles
v <- values(rast)[!is.na(values(rast))]
masque <- quantile(v, 0.10)
values(rast)[values(rast) < masque] <- NA # On rend transparent le vide autour de la ville

pal <- colorBin(c("#c0392b", "#e67e22", "#f1c40f", "#27ae60"), 
                bins=c(masque, quantile(v, 0.25), quantile(v, 0.50), quantile(v, 0.75), max(v)), 
                na.color="transparent")

# Création de la carte de densité
carte_densite <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(rast, colors=pal, opacity=0.6) %>%
  addLegend("bottomright", colors=c("#c0392b", "#e67e22", "#f1c40f", "#27ae60"),
            labels=c("Urgent (vide)", "Faible", "Moyen", "Bien doté"), title="Priorité plantation")

# Sauvegarde html
saveWidget(carte_densite, "carte_densite.html", selfcontained=TRUE)


# =========================================================
# 6. GRAPHIQUE DES QUARTIERS (AVEC POURCENTAGES)
# =========================================================

# Préparation des données par quartier
quartiers <- df_vivants %>%
  filter(!is.na(clc_quartier), clc_quartier != "") %>%
  count(clc_quartier, name="nb_arbres") %>%
  mutate(
    # Calcul des pourcentages pour l'affichage
    pct = round(nb_arbres / sum(nb_arbres) * 100, 1),
    label_pct = paste0(pct, "%"),
    
    # Classification pour les couleurs
    reco = case_when(
      nb_arbres < quantile(nb_arbres, 0.33) ~ "Priorité haute",
      nb_arbres < quantile(nb_arbres, 0.66) ~ "A compléter",
      TRUE ~ "Bien fourni"
    )
  )

# Création du bar chart
graph <- ggplot(quartiers, aes(x=reorder(clc_quartier, nb_arbres), y=nb_arbres, fill=reco)) +
  geom_col() +
  geom_text(aes(label=label_pct), hjust=-0.15, size=3.5) + # Affichage des % au bout des barres
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Laisse de la place pour le texte
  scale_fill_manual(values=c("Priorité haute"="#c0392b", "A compléter"="#f1c40f", "Bien fourni"="#27ae60")) +
  labs(title="Arbres par quartier", x="", y="Nombre d'arbres", fill="Statut") +
  theme_minimal()

# Affichage et sauvegarde
graph
ggsave("quartiers.png", graph, width=10, height=6)