import pandas as pd
from pyproj import Transformer

# Chargement du fichier
file_input = 'Patrimoine_Arboré_data.csv'
df = pd.read_csv(file_input, sep=",", engine='python', encoding='utf-8-sig')

# Nettoyage des noms de colonnes
df.columns = df.columns.str.strip()

# Suppression des colonnes inutiles
cols_to_drop = [
    "Editor", "Creator", "src_geo", "EditDate", "creation_date", 
    "created_user", "GlobalID", "last_edited_user", "last_edited_date", 
    "fk_nomtech", "nomlatin", "id_arbre"
]
df = df.drop(columns=cols_to_drop, errors='ignore')

# Renommage
df = df.rename(columns={"nomfrancais": "nom"})

# Suppression des 11 premières lignes
df = df.iloc[11:].reset_index(drop=True)

# --- CONVERSION ET RÉORGANISATION ---
transformer = Transformer.from_crs("EPSG:3857", "EPSG:4326", always_xy=True)

# 1. Création des nouvelles colonnes
df["lon"], df["lat"] = transformer.transform(df["X"].values, df["Y"].values)

# 2. Suppression des anciennes colonnes X et Y
df = df.drop(columns=["X", "Y"])

# 3. MISE EN DÉBUT : Réorganiser l'ordre des colonnes
# On récupère toutes les colonnes sauf lon et lat
autres_colonnes = [c for c in df.columns if c not in ["lon", "lat"]]
# On définit le nouvel ordre : lon, lat, puis tout le reste
nouvel_ordre = ["lon", "lat"] + autres_colonnes
df = df[nouvel_ordre]

# --- LOGIQUE DE QUARTIER ---
df['clc_quartier'] = df['clc_quartier'].replace(r'^\s*$', pd.NA, regex=True)
reference_data = df.dropna(subset=['clc_quartier'])
mapping = dict(zip(reference_data['clc_secteur'], reference_data['clc_quartier']))

def completer_quartier(row):
    secteur = str(row['clc_secteur'])
    if "le royeux" in secteur.lower():
        return "Zone industrielle le Royeux"
    if pd.isna(row['clc_quartier']):
        return mapping.get(secteur, secteur)
    return row['clc_quartier']

df['clc_quartier'] = df.apply(completer_quartier, axis=1)

# Export final
output_file = 'Patrimoine_Arbore_Nettoye.csv'
df.to_csv(output_file, index=False, sep=';', encoding='utf-8-sig')

print(f"Traitement terminé.")