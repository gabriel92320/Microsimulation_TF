################################################################################
########### FICHIER MAIN POUR LA MICROSIMULATION DE LA TF ######################
################################################################################

################################################################################
# =========== 00 = REGLAGES ET PARAMETRES ======================================
################################################################################

repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Eco_redistribution"


repo_prgm <- paste(repgen, "MicrosimulationTF" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_bases_intermediaires <- paste(repgen, "Bases_intermediaires" , sep = "/")

# Ici quelques paramètres généraux


################################################################################
# =========== 01 = PACKAGES ET SCRIPTS DE FONCTIONS  ===========================
################################################################################
source(paste(repo_prgm , "01_packages.R" , sep = "/"))








################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################
carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))
carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))

# ident = identifiant ménage
# aspa = Allocation solidarité aux personnes âgées
# asi = Allocation supplémentaire d'invalidité
# aah = Allocation aux adultes handicapés
# poi = le poids
# age_pr = Âge personne référence
# decile_ndv = décile niveau de vie
# rfr = revenu fiscal de référence

# https://www.collectivites-locales.gouv.fr/sites/default/files/migration/ffs_2020_bati.pdf = notice pour carac_tf ???
# vlbaia = part de la valeur locative imposée (valeur de l'année)
# bipeva = base d'imposition de la PEV (valeur de l'année)
# bateom = Base d'imposition de la PEV prise en compte pour la taxe d'enlèvement des ordure ménagères
# baomec = Base d'imposition écrêtée de la PEV, càd non prise en compte pour la taxe d'enlèvement des ordures ménagères
# mvltieomx = Montant TIEOM = Taxe d'enlevèment des ordures ménagères ?
# nb_prop = Nb de proprio ?
# ccodep = département
# ccocom = commune
# ccoifp = IFP ??? (le 3ème niveau de collectivité territoriale j'imagine)


nrow(carac_men)

nrow(carac_tf)

merge(carac_tf, carac_men, by.x = "ident21", by.y = "ident")

merge(carac_men,carac_tf,  by.x = "ident", by.y = "ident21")


sum(carac_men$poi)*2 # 16 797 391 ==> https://www.statistiques.developpement-durable.gouv.fr/edition-numerique/chiffres-cles-du-logement-2022/7-proprietaires-occupants#:~:text=D%C3%A9but%202021%2C%2017%2C6%20millions,achat%20de%20leur%20r%C3%A9sidence%20principale. 
# 17.6 millions de ménages sont propriétaires de leur résidence principale en France ==> Ca doit être ça ?


table(carac_men[, .N, by = "ident"]$N) # Chaque identifiant n'apparaît qu'une fois
table(carac_tf[, .N, by = "ident21"]$N) # Certians identifiants apparaîssent plusieurs fois ==> En général même plutôt 2 ou 3 fois

# carac_tf = 1 ligne par bien, ident21 = l'identifiant du ménage qui possède ce bien ==> Sur ça qu'on calcule la taxe foncière



# On merge les deux tables, en gardant TOUS LES BIENS ==> Pour pouvoir récuperer des infos sur les propriétaires
dt_merged <- merge(carac_tf, carac_men, by.x = "ident21", by.y = "ident", all.x = TRUE)


liste_cols_REI <- c("DEPARTEMENT",
                    "DIRECTION",
                    "COMMUNE",
                    "Numéro national du groupement",
                    "NUMERO SIREN DE L'EPCI",
                    "Libellé du Groupement",
                    "option fiscale de l'EPCI (FPA, FPU ou FPZ)",
                    "Forme juridique EPCI (CA, CU, CC, SAN ou Mét)",
                    "Libellé commune",
                    "FB - COMMUNE / TAUX NET",
                    "FB - GFP / TAUX APPLICABLE SUR LE TERRITOIRE DE LA COMMUNE",
                    "FB - GFP / TAUX VOTE",
                    "FB - TSE / TAUX NET")
                    
# "E12", # "Taux de TFB voté par la commune.  Sur le territoire des communes nouvelles concernées par une intégration fiscale progressive (dispositif de convergence des taux suite à une fusion/restructuration), ce taux est recalculé et correspond donc au taux appliqué sur le territoire de la commune. 
# "E32", # "Le taux intercommunal applicable sur le territoire de la commune doit être distingué du taux voté par l'EPCI à fiscalité propre (variable E32VOTE). Le taux applicable est recalculé sur le territoire de la commune. Ainsi, pour les EPCI à fiscalité propre concernés par une intégration fiscale progressive (dispositif de convergence des taux suite à une fusion/restructuration), ce taux applicable sera différent d'une commune membre à l'autre alors que le taux voté est unique.
# "E32VOTE", # "Taux voté par l'EPCI à fiscalité propre.  Il peut être différent du taux applicable sur le territoire de la commune (variable B32) dans les cas d'intégration fiscale progressif (dispositif de convergence des taux)."
# "E52", # "Taux de TSE adossée à la TFB :  A la manière d'un syndicat, l'EPF ne vote pas un taux mais un produit, dans la limite d'un plafond fixé à 20€ par habitant situé dans son périmètre. Le produit est réparti entre toutes les personnes physiques ou morales assujetties aux TF, à la TH et à la CFE dans les communes comprises dans la zone de compétence de l'établissement. Cela permet de déterminer les fractions supplémentaires de taux qui figureront de manière isolée dans les rôles d'imposition des contribuables. 
# "IDCOM")


# REI_21 <- as.data.table(read_excel(path = paste(repo_data, "REI_2021.xlsx", sep = "/"), skip = 1))
# REI_21 <- read_excel(path = paste(repo_data, "REI_2021.xlsx", sep = "/"), col_names = liste_cols_REI)

# REI_21 <- fread(paste(repo_data, "REI_2021.xlsx", sep = "/"), select = liste_cols_REI, head = TRUE)
# REI_21 <- fread(paste(repo_data, "REI_2021.xlsx", sep = "/"), head = TRUE)
chemin_fichier <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Eco_redistribution/Data/REI_2021.xlsx"
# REI_21 <- read_excel(chemin_fichier, col_names = liste_cols_REI)


# REI_21 <- read.xlsx(chemin_fichier, cols = liste_cols_REI)
REI_21 <- read.xlsx(chemin_fichier)
rei <- data.table(REI_21)


# REI_21 <- readxl::read_xlsx(chemin_fichier)
# rei2 <- data.table(REI_21)

nrow(rei2)
length(colnames(rei2))
################################################################################
################### BROUILLON GABRIEL ########################################## 
################################################################################






