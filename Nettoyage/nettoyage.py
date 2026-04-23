import pandas as pd
from pyproj import Transformer
import numpy as np

# =================================================================
# 1. FONCTIONS DE CHARGEMENT ET GÉOMÉTRIE
# =================================================================

def charger_donnees(chemin_fichier):
    """Charge le CSV et effectue le nettoyage de base des colonnes."""
    df = pd.read_csv(chemin_fichier, sep=",", engine='python', encoding='utf-8-sig')
    df.columns = df.columns.str.strip()
    df = df.rename(columns={"nomfrancais": "nom"})
    return df

def convertir_coordonnees(df):
    """Convertit les coordonnées X/Y (EPSG:3949) en Lon/Lat (EPSG:4326)."""
    transformer = Transformer.from_crs("EPSG:3949", "EPSG:4326", always_xy=True)
    df["lon"], df["lat"] = transformer.transform(df["X"].values, df["Y"].values)
    
    # Réorganisation des colonnes pour mettre lon/lat en premier
    df = df.drop(columns=["X", "Y"])
    cols = ["lon", "lat"] + [c for c in df.columns if c not in ["lon", "lat"]]
    return df[cols]

# =================================================================
# 2. FONCTIONS DE LOGIQUE MÉTIER (QUARTIERS)
# =================================================================

def traiter_quartiers(df):
    """Gère les corrections de secteurs et complète les quartiers manquants."""
    # Nettoyage initial
    df['clc_secteur'] = df['clc_secteur'].replace('Griourt', 'Gricourt')
    
    # Création du dictionnaire de référence pour le remplissage
    reference_data = df.dropna(subset=['clc_quartier'])
    mapping = dict(zip(reference_data['clc_secteur'], reference_data['clc_quartier']))

    def appliquer_completer_quartier(row):
        secteur = str(row['clc_secteur'])
        if secteur.lower() == 'nan': return pd.NA
        if "le royeux" in secteur.lower(): return "Zone industrielle le Royeux"
        if pd.isna(row['clc_quartier']):
            res = mapping.get(secteur, secteur)
            return pd.NA if str(res).lower() == 'nan' else res
        return row['clc_quartier']

    df['clc_quartier'] = df.apply(appliquer_completer_quartier, axis=1)
    return df.dropna(subset=['clc_quartier'])

# =================================================================
# 3. FONCTIONS DE TRAITEMENT DES ÂGES ET STADES DE DEVELOPPEMENT
# =================================================================

def traiter_ages_et_stades(df):
    """Estime les âges manquants via le stade et vice-versa."""
    # Nettoyage des types
    df['age_estim'] = pd.to_numeric(df['age_estim'], errors='coerce')
    df = df[(df['age_estim'] <= 1000) | (df['age_estim'].isna())].copy()
    
    df['fk_stadedev'] = df['fk_stadedev'].astype(str).str.lower().str.strip()
    df['fk_stadedev'] = df['fk_stadedev'].replace(['nan', 'none', ''], pd.NA)
    
    # Suppression des lignes inexploitables
    df = df.dropna(subset=['age_estim', 'fk_stadedev'], how='all')

    # Calcul des statistiques de référence
    moyennes_par_stade = df.groupby('fk_stadedev')['age_estim'].mean().to_dict()

    def remplir_age(row):
        if pd.isna(row['age_estim']) and pd.notna(row['fk_stadedev']):
            return moyennes_par_stade.get(row['fk_stadedev'], row['age_estim'])
        return row['age_estim']

    def trouver_stade(age):
        if pd.isna(age): return pd.NA
        if age <= 2: return 'jeune'
        valides = {k: v for k, v in moyennes_par_stade.items() if pd.notna(v)}
        if not valides: return pd.NA
        return min(valides, key=lambda k: abs(valides[k] - age))

    def completer_stade(row):
        val = str(row['fk_stadedev']).lower().strip()
        if pd.isna(row['fk_stadedev']) or val in ['nan', 'none', '', 'pd.na']:
            return trouver_stade(row['age_estim'])
        return row['fk_stadedev']

    df['age_estim'] = df.apply(remplir_age, axis=1)
    df['fk_stadedev'] = df.apply(completer_stade, axis=1)
    return df

# =================================================================
# 4. FONCTIONS DE NETTOYAGE FINAL ET EXPORT
# =================================================================

def finaliser_donnees(df):
    """Traite les dates, les diamètres, gère les colonnes finales et exporte."""
    
    # 1. Gestion des dates en cascade
    for col in ['created_date', 'dte_plantation', 'CreationDate']:
        df[col] = pd.to_datetime(df[col], errors='coerce')
    
    df['created_date'] = df['created_date'].fillna(df['dte_plantation']).fillna(df['CreationDate'])
    
    # 2. Mesures physiques (Diamètre en m)
    df['tronc_diam'] = pd.to_numeric(df['tronc_diam'], errors='coerce') / 100
    moy_adulte = df.loc[df['fk_stadedev'] == 'adulte', 'tronc_diam'].mean()
    df.loc[(df['fk_stadedev'] == 'adulte') & (df['tronc_diam'].isna()), 'tronc_diam'] = moy_adulte
    
    # 3. Arrondis et valeurs par défaut pour les colonnes binaires
    df['tronc_diam'] = df['tronc_diam'].round(3)
    df['age_estim'] = df['age_estim'].round(1)
    df['remarquable'] = df['remarquable'].fillna('Non')
    df['fk_revetement'] = df['fk_revetement'].fillna('Non')
    
    # 4. Suppression des colonnes inutiles
    cols_to_drop = [
        "Editor", "Creator", "src_geo", "EditDate", "CreationDate", "dte_plantation",
        "created_user", "GlobalID", "last_edited_user", "last_edited_date", 
        "fk_nomtech", "nomlatin"
    ]
    return df.drop(columns=cols_to_drop, errors='ignore')

# =================================================================
# EXÉCUTION DU SCRIPT PRINCIPAL
# =================================================================

if __name__ == "__main__":
    print("Démarrage du traitement...")
    
    data = charger_donnees('Patrimoine_Arboré_data.csv')
    data = convertir_coordonnees(data)
    
    # Filtrage sur mesures essentielles
    colonnes_essentielles = ['haut_tot', 'tronc_diam', 'haut_tronc']
    data = data.dropna(subset=colonnes_essentielles)
    
    data = traiter_quartiers(data)
    data = traiter_ages_et_stades(data)
    data = finaliser_donnees(data)
    
    # Export
    data.to_csv('Patrimoine_Arbore_Nettoye.csv', index=False, sep=';', encoding='utf-8-sig')
    
    print(f"--- TRAITEMENT TERMINÉ ---")
    print(f"Nombre d'arbres traités : {len(data)}")