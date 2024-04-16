################################################################################
########### PROGRAMMES POUR LE CHARGEMENT DES PACKAGES #########################
################################################################################

################################################################################
# =========== 00 = REGLAGES ET PARAMETRES ======================================
################################################################################

repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Eco_redistribution"
repo_prgm <- paste(repgen, "/Microsimulation_TF" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_bases_intermediaires <- paste(repgen, "Bases_intermediaires" , sep = "/")

# Ici quelques paramètres généraux

################################################################################
# =========== 01 = PACKAGES ET PRGM ANNEXES  ===================================
################################################################################

# Là on importe les scripts de fonctions, par exemple :
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "02_Import_et_creation_bases.R" , sep = "/"))
source(paste(repo_prgm , "03_Preparation_graphiques.R" , sep = "/"))
source(paste(repo_prgm , "03_Graphiques.R" , sep = "/"))
source(paste(repo_prgm , "03_Sous_fonctions_diverses.R" , sep = "/"))
source(paste(repo_prgm , "04_Econometrie.R" , sep = "/"))
