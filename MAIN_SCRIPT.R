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
source(paste(repo_prgm , "02_Importations_preparation.R" , sep = "/"))

################################################################################
# ================= 02 = PREPARATION DES DT  ===================================
################################################################################

liste_cols_REI <- c("DEPARTEMENT",
                    "DIRECTION",
                    "COMMUNE",
                    "Numéro.national.du.groupement",
                    "NUMERO.SIREN.DE.L'EPCI",
                    "Libellé.du.Groupement",
                    "option.fiscale.de.l'EPCI.(FPA,.FPU.ou.FPZ)",
                    "Forme.juridique.EPCI.(CA,.CU,.CC,.SAN.ou.Mét)",
                    "Libellé.commune",
                    "FB.-.FRAIS.D'ASSIETTE,.DEGREVEMENT,.NON.VALEURS",
                    "FB.-.COMMUNE./.BASE.NETTE",
                    "FB.-.COMMUNE./.TAUX.NET",
                    "FB.-.COMMUNE./.MONTANT.REEL",
                    "FB.-.COMMUNE./.NOMBRE.D'ARTICLES",
                    "FB.-.COMMUNE./.MONTANT.LISSAGE",
                    "FB.-.GFP./.BASE.NETTE",
                    "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE",
                    "FB.-.GFP./.TAUX.VOTE",
                    "FB.-.GFP./.MONTANT.REEL",
                    "FB.-.GFP./.MONTANT.LISSAGE",
                    "FB.-.TSE./.BASE.NETTE",
                    "FB.-.TSE./.TAUX.NET")

# Faire_dt_SOUS_REI(liste_cols_REI_loc) # A DECOMMENTER LA PREMIERE FOIS : Importe le REI, ne sélectionne que les colonnes de liste_cols_REI_loc et sauvegarde

liste_cols_REI_loc <- c("DEPARTEMENT",
                       "COMMUNE",
                       "Numéro.national.du.groupement",
                       "FB.-.COMMUNE./.TAUX.NET",
                       "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE",
                       "FB.-.TSE./.TAUX.NET", 
                       "Libellé.commune")

dt_merged_REI <- Importer_et_merge_DT_REI(liste_cols_REI_loc)

dt_merged_REI[is.na(FB_TSE_TAUX_NET)] # Bon on n'a pas les outre mer...
100*nrow(dt_merged_REI[is.na(FB_TSE_TAUX_NET)])/nrow(dt_merged_REI) # Bon ça fait 0.21% des observations pas très grave sans doute

################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################



# # Puis Merge merged et REI
# dt_merged_REI <- merge(dt_merged, SOUS_REI, by.x = c("ccodep", "ccocom"), by.y = c("DEPARTEMENT", "COMMUNE"), all.x = TRUE)
# 
# nrow(dt_merged)
# nrow(dt_merged_REI)
# 
# table(dt_merged_REI[is.na(FB_TSE_TAUX_NET)]$ccodep) # Il trouve pas Paris Marseille Lyon...
# 
# 
# SOUS_REI[DEPARTEMENT == 69 & Libellé.commune %like% "LYON"]
# SOUS_REI[DEPARTEMENT == 13 & Libellé.commune %like% "MARSEILLE"]
# SOUS_REI[DEPARTEMENT == 75 & Libellé.commune %like% "PARIS"]
# 
# table(carac_tf[ccodep== 69]$ccocom)
# 
# liste_num_Lyon <- 381:389
# liste_num_Marseille <- 201:216
# liste_num_Paris <- 101:120
#   
# carac_tf[, ccocom_REI := ccocom]
# carac_tf[ccodep == "69" & ccocom_REI %in% liste_num_Lyon, ccocom_REI := "123"] # On met le code de la commune pour avoir le REU, et pas le code des arrondissement
# carac_tf[ccodep == "13" & ccocom_REI %in% liste_num_Marseille, ccocom_REI := "055"] 
# carac_tf[ccodep == "75" & ccocom_REI %in% liste_num_Paris, ccocom_REI := "056"]
# 
# # Merge TF et MEN
# dt_merged <- merge(carac_tf, carac_men, by.x = "ident21", by.y = "ident")
# 
# # Puis Merge merged et REI
# dt_merged_REI <- merge(dt_merged, SOUS_REI, by.x = c("ccodep", "ccocom_REI"), by.y = c("DEPARTEMENT", "COMMUNE"), all.x = TRUE)
# 
# nrow(dt_merged)
# nrow(dt_merged_REI)
# 
# table(dt_merged_REI[is.na(FB_TSE_TAUX_NET)]$ccodep)
# dt_merged_REI[is.na(FB_TSE_TAUX_NET)]



# Lyon 1er Arrondissement (69381)
# Lyon 2e Arrondissement (69382)
# Lyon 3e Arrondissement (69383)
# Lyon 4e Arrondissement (69384)
# Lyon 5e Arrondissement (69385)
# Lyon 6e Arrondissement (69386)
# Lyon 7e Arrondissement (69387)
# Lyon 8e Arrondissement (69388)
# Lyon 9e Arrondissement (69389)

# Marseille 1er Arrondissement (13201)
# Marseille 2e Arrondissement (13202)
# Marseille 3e Arrondissement (13203)
# Marseille 4e Arrondissement (13204)
# Marseille 5e Arrondissement (13205)
# Marseille 6e Arrondissement (13206)
# Marseille 7e Arrondissement (13207)
# Marseille 8e Arrondissement (13208)
# Marseille 9e Arrondissement (13209)
# Marseille 10e Arrondissement (13210)
# Marseille 11e Arrondissement (13211)
# Marseille 12e Arrondissement (13212)
# Marseille 13e Arrondissement (13213)
# Marseille 14e Arrondissement (13214)
# Marseille 15e Arrondissement (13215)
# Marseille 16e Arrondissement (13216)

# Paris 1er Arrondissement (75101)
# Paris 2e Arrondissement (75102)
# Paris 3e Arrondissement (75103)
# Paris 4e Arrondissement (75104)
# Paris 5e Arrondissement (75105)
# Paris 6e Arrondissement (75106)
# Paris 7e Arrondissement (75107)
# Paris 8e Arrondissement (75108)
# Paris 9e Arrondissement (75109)
# Paris 10e Arrondissement (75110)
# Paris 11e Arrondissement (75111)
# Paris 12e Arrondissement (75112)
# Paris 13e Arrondissement (75113)
# Paris 14e Arrondissement (75114)
# Paris 15e Arrondissement (75115)
# Paris 16e Arrondissement (75116)
# Paris 17e Arrondissement (75117)
# Paris 18e Arrondissement (75118)
# Paris 19e Arrondissement (75119)
# Paris 20e Arrondissement (75120)



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




# liste_cols_REI <- c("DEP", # DEPARTEMENT
#                     "DIR", # DIRECTION
#                     "COM", # COMMUNE
#                     "Q02", # Numero national du Groupement
#                     "SIREPCI", # Numéro SIREN de l'EPCI
#                     "Q03", # Libellé du Groupement
#                     "OPTEPCI", # Option fiscale de l'EPCI (FPA, FPU ou FPZ)
#                     "FORJEPCI", # Forme jurisique EPCI (CA, CU, CC, SAN ou Mét)
#                     "LIBCOM", # Commune
#                     "E00", # FB - FRAIS D'ASSIETTE, DEGREVEMENT,  NON VALEURS
#                     "E11", # FB - COMMUNE / BASE NETTE
#                     "E12", # FB - COMMUNE / TAUX NET = "Taux de TFB net voté par la commune.  Sur le territoire des communes nouvelles concernées par une intégration fiscale progressive (dispositif de convergence des taux suite à une fusion/restructuration), ce taux est recalculé et correspond donc au taux appliqué sur le territoire de la commune.
#                     "E13", # FB - COMMUNE / MONTANT REEL
#                     "E14", # FB - COMMUNE / NOMBRE D'ARTICLES
#                     "E16", # FB - COMMUNE / MONTANT LISSAGE
#                     "E31", # FB - GFP / BASE NETTE
#                     "E32", # FB - GFP / TAUX APPLICABLE SUR LE TERRITOIRE DE LA COMMUNE = "Le taux intercommunal applicable sur le territoire de la commune doit être distingué du taux voté par l'EPCI à fiscalité propre (variable E32VOTE). Le taux applicable est recalculé sur le territoire de la commune. Ainsi, pour les EPCI à fiscalité propre concernés par une intégration fiscale progressive (dispositif de convergence des taux suite à une fusion/restructuration), ce taux applicable sera différent d'une commune membre à l'autre alors que le taux voté est unique.
#                     "E32VOTE", # FB - GFP / TAUX VOTE =  "Taux voté par l'EPCI à fiscalité propre.  Il peut être différent du taux applicable sur le territoire de la commune (variable B32) dans les cas d'intégration fiscale progressif (dispositif de convergence des taux)."
#                     "E33", # FB - GFP / MONTANT REEL
#                     "E36", # FB - GFP / MONTANT LISSAGE
#                     "E51", # FB - TSE / BASE NETTE
#                     "E52", # FB - TSE / TAUX NET = "Taux de TSE adossée à la TFB :  A la manière d'un syndicat, l'EPF ne vote pas un taux mais un produit, dans la limite d'un plafond fixé à 20€ par habitant situé dans son périmètre. Le produit est réparti entre toutes les personnes physiques ou morales assujetties aux TF, à la TH et à la CFE dans les communes comprises dans la zone de compétence de l'établissement. Cela permet de déterminer les fractions supplémentaires de taux qui figureront de manière isolée dans les rôles d'imposition des contribuables.
#                     "IDCOM")
                    

colnames(REI_21)



# REI_21 <- as.data.table(read_excel(path = paste(repo_data, "REI_2021.xlsx", sep = "/"), skip = 1))
# REI_21 <- read_excel(path = paste(repo_data, "REI_2021.xlsx", sep = "/"), col_names = liste_cols_REI)

# REI_21 <- fread(paste(repo_data, "REI_2021.xlsx", sep = "/"), select = liste_cols_REI, head = TRUE)
# REI_21 <- fread(paste(repo_data, "REI_2021.xlsx", sep = "/"), head = TRUE)
chemin_fichier <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Eco_redistribution/Data/REI_2021.xlsx"
# REI_21 <- read_excel(chemin_fichier, col_names = liste_cols_REI)



# REI_21 <- read.xlsx(chemin_fichier, cols = liste_cols_REI)


# REI_21 <- readxl::read_xlsx(chemin_fichier)
# rei2 <- data.table(REI_21)

nrow(rei)
length(colnames(rei))

sous_rei <- rei[, ..liste_cols_REI] 
  
  


################################################################################
################### BROUILLON GABRIEL ########################################## 
################################################################################






