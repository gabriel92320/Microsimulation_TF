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
                       "FB.-.COMMUNE./.TAUX.NET", # Le Tx communal 
                       "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", # Le Tx GFP = EPCI ?
                       "FB.-.TSE./.TAUX.NET", # TAXE SPECIALE D'EQUIPEMENT 
                       "Libellé.commune")

dt_merged_REI <- Importer_et_merge_DT_REI(liste_cols_REI_loc)

dt_merged_REI[is.na(FB_TSE_TAUX_NET)] # Bon on n'a pas les outre mer...
100*nrow(dt_merged_REI[is.na(FB_TSE_TAUX_NET)])/nrow(dt_merged_REI) # Bon ça fait 0.21% des observations pas très grave sans doute

################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################
######### DOCUMENTATION A METTRE SUR GIT #######################################

# GFP = EPCI 
# Le taux par département était là en 2016, mais pas en 2021... 
# https://www.impots.gouv.fr/sites/default/files/media/8_transverse/open_data/tf/nid_13063_algorithme_tf.pdf ==> Il n'y a plus que des taux par communes et par EPCI ?

l <- c("ccocom", "ccoifp")
dt_merged_REI[ccodep == "83"][,..l]

table(dt_merged_REI$ccocom == dt_merged_REI$ccoifp)
dt_merged_REI[ccocom != ccoifp]

dt_merged_REI[, .N, by = "Numéro_national_du_groupement"]
dt_merged_REI[Numéro_national_du_groupement == "010002"]$FB_TSE_TAUX_NET

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





################################################################################
################### BROUILLON GABRIEL ########################################## 
################################################################################






