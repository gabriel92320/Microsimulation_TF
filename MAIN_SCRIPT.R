################################################################################
########### FICHIER MAIN POUR LA MICROSIMULATION DE LA TF ######################
################################################################################

## A faire ##

# Ajouter le tx départemental dans le calcul de la TF en 2020
# Piste de qq graphiques : regarder l'évolution 2020, 2021 et 2022 
# ==> Toutes les fonctions d'importations ont été adaptée pour 2020, 2021 et 2022. Il suffit de mettre REI_XXXX dans le dossier data et de faire tourner la ligne "A DECOMMENTER"

# Regarder l'effet "évolution des taux 21-22" sous d'autres angles: classe d'âge, par région, par type d'unité urbaine ?

# Récupérer les zonages TUU 2020 et les apparier à la base "logements"
# Coder une variable catégorielle de classe d'âge

# Implémentation des exonérations de TFPB:
# Distinguer exonération totale/exonération partielle


################################################################################
# =========== 00 = REGLAGES ET PARAMETRES ======================================
################################################################################

#repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Eco_redistribution"
 repgen <- "/Users/gabrielsklenard/Documents/Memoire_Microsimulation"


repo_prgm <- paste(repgen, "MicrosimulationTF" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_bases_intermediaires <- paste(repgen, "Bases_intermediaires" , sep = "/")

# Ici quelques paramètres généraux
annee <- 2021 # Pour 2020 il y a le tx départemental en plus
toutes_annees <- TRUE # Pour calculer les montants sur les 3 années 2020, 2021 et 2022

mettre_titres_graphiques <- TRUE # Pour sauvegarder les graphes SANS leur titre (pour pouvoir mettre le titre en caption latex)
utiliser_dvldif2a <- FALSE # Pour utiliser la variable dvldif2a = Montant de VL exonérée (valeur de l’année) pour le calcul TF net ==> Ne change pas grand chose, mais je pense que c'est plus propre parce que déjà contenu dans la base

nb_quantiles <- 20 # Pour tracer la TF en fnt des quantiles de RFR

rfr_min <- 1500 # rfr minimum pour ne pas faire des ratio trop gros
################################################################################
# =========== 01 = PACKAGES ET SCRIPTS DE FONCTIONS  ===========================
################################################################################
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "02_Importations_preparation.R" , sep = "/"))
source(paste(repo_prgm , "03_Calcul_taux_TF.R" , sep = "/"))
source(paste(repo_prgm , "04_Preparation_graphiques.R" , sep = "/"))
source(paste(repo_prgm , "05_Graphiques.R" , sep = "/"))

################################################################################
# ================= 02 = PREPARATION DES DT sur 1 an ===========================
################################################################################

liste_cols_REI_loc <- c("DEPARTEMENT",
                    "DIRECTION",
                    "COMMUNE",
                    "Numéro.national.du.groupement",
                    # "NUMERO.SIREN.DE.L'EPCI",
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

annee_loc <- annee
# Faire_dt_SOUS_REI(liste_cols_REI_loc, annee_loc) # A DECOMMENTER LA PREMIERE FOIS : Importe le REI, ne sélectionne que les colonnes de liste_cols_REI_loc et sauvegarde

liste_cols_REI_loc <- c("DEPARTEMENT",
                       "COMMUNE",
                       "Numéro.national.du.groupement",
                       "FB.-.COMMUNE./.TAUX.NET", # Le Tx communal 
                       "FB.-.GFP./.TAUX.VOTE", # Le taux pour le montant brut à l'échelle du GFP = EPCI
                       "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", # Le tx pour montant net
                       "Libellé.commune")

if(annee == 2020){ # En 2020 il faut ajouter le tx du dpt
  liste_cols_REI_loc <- append(liste_cols_REI_loc, "FB.-.DEP./.TAUX.NET")
}

annee_loc <- annee
# On merge REI et carac_tf, on est donc à l'échelle du LOGEMENT
dt_merged_REI <- Importer_et_merge_REI_carac_tf(liste_cols_REI_loc, annee_loc)

# On calcule la TF brute par logement
dt_merged_REI_loc <- copy(dt_merged_REI)
annee_loc <- annee
dt_merged_REI <- Calculer_taux_brut(dt_merged_REI_loc, annee_loc)
  
# On importe aussi carac_men et caract_tf
carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))
carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))

# On calcule la TF nette par logement
carac_men_loc <- copy(carac_men)
dt_merged_REI_loc <- copy(dt_merged_REI)
annee_loc <- annee
dt_merged_REI <- Calculer_taux_net(dt_merged_REI_loc, carac_men_loc, annee_loc, utiliser_dvldif2a)


# On stocke tous les chemins des pdf générés, pour pouvoir les fusionner à la fin et obtenir un joli cahier graphique
liste_chemins_graphes <- c()



################################ GRAPHIQUES ####################################


# On commence par préparer le dt nécessaire à tracer le graphe
dt_merged_REI_loc <- copy(dt_merged_REI)
var_montant_TF <- "Montant_TF_NETTE_proratise"

# Ventilation de la TFPB nette totale (en Md d'euros) et moyenne (en euros):

# a. Par décile de niveau de vie des ménages 2021:
TFPB_nette_decile_ndv_p <- Calcul_montant_tot_moy_TFPB_nivviem(dt_merged_REI_loc, var_montant_TF)
# b. Par type de la commune (UU / hors UU): 
#ATTENTION SEULES LES RESIDENCES PRINCIPALES SONT PRISES EN COMPTE ICI!!
TFPB_nette_type_com_UU_p <- Calcul_montant_tot_moy_TFPB_type_com_UU(dt_merged_REI_loc, var_montant_TF)
# c. Par statut de la commune (hors UU/Ville-centre/Banlieue/Ville isolée): 
#ATTENTION SEULES LES RESIDENCES PRINCIPALES SONT PRISES EN COMPTE ICI!!
TFPB_nette_statut_com_UU_p <- Calcul_montant_tot_moy_TFPB_statut_com_UU(dt_merged_REI_loc, var_montant_TF)

print(xtable(TFPB_nette_statut_com_UU_p), include.rownames = FALSE)


####### PARTIE 1 : MONTANT TOTAL DE LA TF NETTE
#TFPB nette totale (en Md euros) par décile de niveau de vie des ménages en 2021

# On prépare tous les arguments 
data_loc <- copy(TFPB_nette_decile_ndv_p)
x <- "decile_ndv"
y <- "TFPB_tot"
xlabel <- "Déciles de niveau de vie"
ylabel <- "TFPB"
ysuffix <- "M€"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Montant_tot_TFPB_", annee, ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Montant total de TFPB nette en 2021"
sous_titre_graphe <- "Distribution selon le niveau de vie des ménages propriétaires"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot(data_loc, x, y,xlabel, ylabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)


####### PARTIE 2 : MONTANT MOYEN DE LA TF NETTE
#TFPB nette moyenne (en Md euros) par décile de niveau de vie des ménages en 2021

# On prépare tous les arguments 
data_loc <- copy(TFPB_nette_decile_ndv_p)
x <- "decile_ndv"
y <- "TFPB_moy"
xlabel <- "Déciles de niveau de vie"
ylabel <- "TFPB"
ysuffix <- "€"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Montant_moy_TFPB_", annee, ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Montant moyen de TFPB nette en 2021"
sous_titre_graphe <- "Distribution selon le niveau de vie des ménages propriétaires"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot(data_loc, x, y,xlabel, ylabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)



####### PARTIE 3 : ASSIETTE ET TAUX APPARENTS

# On récupère assiettes et tx apparents par ménage
var_montant_TF <- "Montant_TF_NETTE_proratise"
dt_merged_REI_loc <- copy(dt_merged_REI)
TFPB_menages_tb <- Calcul_assiette_tx_apparent_TFPB(dt_merged_REI_loc, var_montant_TF)


# Calcul du taux moyen apparent par décile niveau de vie:
tx_moyen_apparent_decile_ndv <- TFPB_menages_tb %>%
  group_by(decile_ndv) %>%
  summarise(TFPB=sum(TFPB*poi2,na.rm = T),
            assiette_TFPB = sum(assiette_TFPB*poi2,na.rm = T)) %>%
  mutate(tx_apparent_moy_TFPB_b=TFPB/assiette_TFPB*100) %>%
  mutate(assiette_TFPB_mde=assiette_TFPB/1e9)

tx_moyen_apparent_decile_ndv <- tx_moyen_apparent_decile_ndv %>%
  filter(!(is.na(decile_ndv))) %>%
  mutate(decile_ndv = factor(decile_ndv)) %>%
  mutate(decile_ndv=fct_recode(decile_ndv,"D1"="1","D2"="2","D3"="3","D4"="4",
                               "D5"="5","D6"="6","D7"="7","D8"="8","D9"="9",
                               "D10"="10"))

#### Puis graphique :
# On prépare tous les arguments 
data_loc <- copy(tx_moyen_apparent_decile_ndv)
x <- "decile_ndv"
y <- "tx_apparent_moy_TFPB_b"
xlabel <- "Déciles de niveau de vie"
ylabel <- "Taux moyen apparent"
ysuffix <- "%"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Moyenne_tx_apparents_TFPB_", annee, ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Moyenne des taux apparents de TFPB nette en 2021"
sous_titre_graphe <- "Distribution selon le niveau de vie des ménages propriétaires"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot(data_loc, x, y,xlabel, ylabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)


#### Puis graphique de l'assiette
# On prépare tous les arguments 
data_loc <- copy(tx_moyen_apparent_decile_ndv)
x <- "decile_ndv"
y <- "assiette_TFPB_mde"
xlabel <- "Déciles de niveau de vie"
ylabel <- "Assiette"
ysuffix <- "M€"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Assiette_TFPB_", annee, ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Assiette de la TFPB nette en 2021"
sous_titre_graphe <- "Distribution par décile de niveau de vie"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot(data_loc, x, y,xlabel, ylabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)





################################################################################
# ================= PREPARATION DES DT sur 3 ans ===============================
################################################################################
liste_cols_REI_loc <- c("DEPARTEMENT",
                        "COMMUNE",
                        "Numéro.national.du.groupement",
                        "FB.-.COMMUNE./.TAUX.NET", # Le Tx communal 
                        "FB.-.GFP./.TAUX.VOTE", # Le taux pour le montant brut à l'échelle du GFP = EPCI
                        "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", # Le tx pour montant net
                        "Libellé.commune")

carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))
carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))


# Importation 2020
liste_cols_REI_loc_20 <- append(liste_cols_REI_loc, "FB.-.DEP./.TAUX.NET")
annee_loc <- 2020
dt_merged_REI <- Importer_et_merge_REI_carac_tf(liste_cols_REI_loc_20, annee_loc)
dt_merged_REI <- Calculer_taux_brut(copy(dt_merged_REI), annee_loc)
dt_merged_REI <- Calculer_taux_net(copy(dt_merged_REI), copy(carac_men), annee_loc, utiliser_dvldif2a)
dt_merged_REI_2020 <- copy(dt_merged_REI)

# Importation 2021
annee_loc <- 2021
dt_merged_REI <- Importer_et_merge_REI_carac_tf(liste_cols_REI_loc, annee_loc)
dt_merged_REI <- Calculer_taux_brut(copy(dt_merged_REI), annee_loc)
dt_merged_REI <- Calculer_taux_net(copy(dt_merged_REI), copy(carac_men), annee_loc, utiliser_dvldif2a)
dt_merged_REI_2021 <- copy(dt_merged_REI)
dt_merged_REI_2021$Montant_DEP_TF <- 0 # Pour pouvoir comparer à 2020

# Importation 2022
annee_loc <- 2022
dt_merged_REI <- Importer_et_merge_REI_carac_tf(liste_cols_REI_loc, annee_loc)
dt_merged_REI <- Calculer_taux_brut(copy(dt_merged_REI), annee_loc)
dt_merged_REI <- Calculer_taux_net(copy(dt_merged_REI), copy(carac_men), annee_loc, utiliser_dvldif2a)
dt_merged_REI_2022 <- copy(dt_merged_REI)
dt_merged_REI_2022$Montant_DEP_TF <- 0 # Pour pouvoir comparer à 2020



# La liste des colonnes qu'on veut indicer par l'année et conserver
liste_cols_annee <- c("FB_COMMUNE_TAUX_NET", "FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE", "Montant_TF_BRUT_proratise", "Montant_TF_NETTE_proratise", "Montant_DEP_TF")
# La liste des colonnes qu'on ne va pas bouger
liste_cols_immobiles <- colnames(carac_tf)
liste_cols_immobiles_2 <- c("LIBGEO","UU2020","LIBUU2020","TYPE_COMMUNE_UU","STATUT_COM_UU")
liste_cols_immobiles <- append(liste_cols_immobiles, liste_cols_immobiles_2)

# Pour s'éviter les problèmes ensuite : on remet dt_merged_REI = dt_merged_REI_2021
dt_merged_REI <- copy(dt_merged_REI_2021)

dt_merged_REI_suivi <- dt_merged_REI_2022[,..liste_cols_immobiles]

# On renome les variables qu'on bouge et on les met dans notre nouveau dt de travail
for(var in liste_cols_annee){
  # 2020
  nv_nom <- paste(var, "2020", sep = "_")
  setnames(dt_merged_REI_2020, var, nv_nom)
  dt_merged_REI_suivi[[nv_nom]] <- dt_merged_REI_2020[[nv_nom]]
  
  nv_nom <- paste(var, "2021", sep = "_")
  setnames(dt_merged_REI_2021, var, nv_nom)
  dt_merged_REI_suivi[[nv_nom]] <- dt_merged_REI_2021[[nv_nom]]
  
  nv_nom <- paste(var, "2022", sep = "_")
  setnames(dt_merged_REI_2022, var, nv_nom)
  dt_merged_REI_suivi[[nv_nom]] <- dt_merged_REI_2022[[nv_nom]]
}



dt_merged_REI_suivi # Le dt avec 3 années



################################################################################
# ===================== GRAPHIQUES SUR 3 ANS  ==================================
################################################################################


# Les 3 tables sur 3 années
dt_merged_REI_loc <- copy(dt_merged_REI_suivi)
var_montant_TF <- "Montant_TF_NETTE_proratise_2020"
TFPB_nette_decile_ndv_p_20 <- data.table(Calcul_montant_tot_moy_TFPB_nivviem(dt_merged_REI_loc, var_montant_TF))
TFPB_nette_decile_ndv_p_20$annee <- 2020

var_montant_TF <- "Montant_TF_NETTE_proratise_2021"
TFPB_nette_decile_ndv_p_21 <- data.table(Calcul_montant_tot_moy_TFPB_nivviem(dt_merged_REI_loc, var_montant_TF))
TFPB_nette_decile_ndv_p_21$annee <- 2021

var_montant_TF <- "Montant_TF_NETTE_proratise_2022"
TFPB_nette_decile_ndv_p_22 <- data.table(Calcul_montant_tot_moy_TFPB_nivviem(dt_merged_REI_loc, var_montant_TF))
TFPB_nette_decile_ndv_p_22$annee <- 2022

# rbind pour obtenir 1 table concaténée
TFPB_nette_decile_ndv_p <- rbindlist(list(TFPB_nette_decile_ndv_p_20, TFPB_nette_decile_ndv_p_21, TFPB_nette_decile_ndv_p_22))
TFPB_nette_decile_ndv_p$annee <- as.factor(TFPB_nette_decile_ndv_p$annee)



####### PARTIE 1 : MONTANT TOTAL DE LA TF NETTE
#TFPB nette totale (en Md euros) par décile de niveau de vie des ménages en 2021
data_loc <- copy(TFPB_nette_decile_ndv_p)
x <- "decile_ndv"
y <- "TFPB_tot"
fill <- "annee"
xlabel <- "Déciles de niveau de vie"
ylabel <- "TFPB"
ysuffix <- "M€"
filllabel <- "Année"

titre_save <- paste("Montant_tot_TFPB_Toutes_annees", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

titre_graphe <- "Montant total de TFPB nette"
sous_titre_graphe <- "Distribution selon le niveau de vie des ménages propriétaires"
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

Faire_graphique_barplot_avec_fill(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix = "M€", titre_save, titre_graphe, sous_titre_graphe)



####### PARTIE 2 : MONTANT MOYEN DE LA TF NETTE
#TFPB nette moyenne (en Md euros) par décile de niveau de vie des ménages en 2021

# On prépare tous les arguments 
y <- "TFPB_moy"
ysuffix <- "€"

titre_save <- paste("Montant_moy_TFPB_Toutes_annees", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

titre_graphe <- "Montant moyen de TFPB nette"
sous_titre_graphe <- "Distribution selon le niveau de vie des ménages propriétaires"

if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

Faire_graphique_barplot_avec_fill(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)




####### PARTIE 3 : ASSIETTE ET TAUX APPARENTS
# On récupère assiettes et tx apparents par ménage
var_montant_TF <- "Montant_TF_NETTE_proratise_2020"
TFPB_menages_tb <- Calcul_assiette_tx_apparent_TFPB(copy(dt_merged_REI_suivi), var_montant_TF)
tx_moyen_apparent_decile_ndv <- TFPB_menages_tb %>%
  group_by(decile_ndv) %>%
  summarise(TFPB=sum(TFPB*poi2,na.rm = T),
            assiette_TFPB = sum(assiette_TFPB*poi2,na.rm = T)) %>%
  mutate(tx_apparent_moy_TFPB_b=TFPB/assiette_TFPB*100) %>%
  mutate(assiette_TFPB_mde=assiette_TFPB/1e9)

tx_moyen_apparent_decile_ndv <- tx_moyen_apparent_decile_ndv %>%
  filter(!(is.na(decile_ndv))) %>%
  mutate(decile_ndv = factor(decile_ndv)) %>%
  mutate(decile_ndv=fct_recode(decile_ndv,"D1"="1","D2"="2","D3"="3","D4"="4",
                               "D5"="5","D6"="6","D7"="7","D8"="8","D9"="9",
                               "D10"="10"))
tx_moyen_apparent_decile_ndv_2020 <- data.table(tx_moyen_apparent_decile_ndv)
tx_moyen_apparent_decile_ndv_2020$annee <- 2020



var_montant_TF <- "Montant_TF_NETTE_proratise_2021"
TFPB_menages_tb <- Calcul_assiette_tx_apparent_TFPB(copy(dt_merged_REI_suivi), var_montant_TF)
tx_moyen_apparent_decile_ndv <- TFPB_menages_tb %>%
  group_by(decile_ndv) %>%
  summarise(TFPB=sum(TFPB*poi2,na.rm = T),
            assiette_TFPB = sum(assiette_TFPB*poi2,na.rm = T)) %>%
  mutate(tx_apparent_moy_TFPB_b=TFPB/assiette_TFPB*100) %>%
  mutate(assiette_TFPB_mde=assiette_TFPB/1e9)

tx_moyen_apparent_decile_ndv <- tx_moyen_apparent_decile_ndv %>%
  filter(!(is.na(decile_ndv))) %>%
  mutate(decile_ndv = factor(decile_ndv)) %>%
  mutate(decile_ndv=fct_recode(decile_ndv,"D1"="1","D2"="2","D3"="3","D4"="4",
                               "D5"="5","D6"="6","D7"="7","D8"="8","D9"="9",
                               "D10"="10"))
tx_moyen_apparent_decile_ndv_2021 <- data.table(tx_moyen_apparent_decile_ndv)
tx_moyen_apparent_decile_ndv_2021$annee <- 2021




var_montant_TF <- "Montant_TF_NETTE_proratise_2022"
TFPB_menages_tb <- Calcul_assiette_tx_apparent_TFPB(copy(dt_merged_REI_suivi), var_montant_TF)
tx_moyen_apparent_decile_ndv <- TFPB_menages_tb %>%
  group_by(decile_ndv) %>%
  summarise(TFPB=sum(TFPB*poi2,na.rm = T),
            assiette_TFPB = sum(assiette_TFPB*poi2,na.rm = T)) %>%
  mutate(tx_apparent_moy_TFPB_b=TFPB/assiette_TFPB*100) %>%
  mutate(assiette_TFPB_mde=assiette_TFPB/1e9)

tx_moyen_apparent_decile_ndv <- tx_moyen_apparent_decile_ndv %>%
  filter(!(is.na(decile_ndv))) %>%
  mutate(decile_ndv = factor(decile_ndv)) %>%
  mutate(decile_ndv=fct_recode(decile_ndv,"D1"="1","D2"="2","D3"="3","D4"="4",
                               "D5"="5","D6"="6","D7"="7","D8"="8","D9"="9",
                               "D10"="10"))
tx_moyen_apparent_decile_ndv_2022 <- data.table(tx_moyen_apparent_decile_ndv)
tx_moyen_apparent_decile_ndv_2022$annee <- 2022




tx_moyen_apparent_decile_ndv <- rbindlist(list(tx_moyen_apparent_decile_ndv_2020, tx_moyen_apparent_decile_ndv_2021, tx_moyen_apparent_decile_ndv_2022))
tx_moyen_apparent_decile_ndv$annee <- as.factor(tx_moyen_apparent_decile_ndv$annee)


#### Puis graphique :
# On prépare tous les arguments 
data_loc <- copy(tx_moyen_apparent_decile_ndv)
x <- "decile_ndv"
y <- "tx_apparent_moy_TFPB_b"
fill <- 'annee'
xlabel <- "Déciles de niveau de vie"
ylabel <- "Taux moyen apparent"
ysuffix <- "%"
filllabel <- "Année"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Moyenne_tx_apparents_TFPB_Toutes_annees", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Moyenne des taux apparents de TFPB nette"
sous_titre_graphe <- "Distribution selon le niveau de vie des ménages propriétaires"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot_avec_fill(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)


#### Puis graphique de l'assiette
# On prépare tous les arguments 
y <- "assiette_TFPB_mde"
ylabel <- "Assiette"
ysuffix <- "M€"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Assiette_TFPB_Toutes_annees", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Assiette de la TFPB nette"
sous_titre_graphe <- "Distribution par décile de niveau de vie"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot_avec_fill(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)





####### PARTIE 4 : EN FRACTION DU RFR
# Ce que chaque ménage a payé de TF en 2020, 2021 et 2022
carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_suivi$ident21 <- as.factor(dt_merged_REI_suivi$ident21)

TF_men_2020 <- dt_merged_REI_suivi[, sum(Montant_TF_NETTE_proratise_2020), by = "ident21"]
setnames(TF_men_2020, "V1", "TF_nette_2020")
TF_men_2021 <- dt_merged_REI_suivi[, sum(Montant_TF_NETTE_proratise_2021), by = "ident21"]
setnames(TF_men_2021, "V1", "TF_nette_2021")
TF_men_2022 <- dt_merged_REI_suivi[, sum(Montant_TF_NETTE_proratise_2022), by = "ident21"]
setnames(TF_men_2022, "V1", "TF_nette_2022")

TF_men <- merge(TF_men_2020, TF_men_2021, by = "ident21")
TF_men <- merge(TF_men, TF_men_2022, by = "ident21")

merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")




merged[, Part_TF_rfr_2020 := 100*TF_nette_2020/rfr]
merged[, Part_TF_rfr_2021 := 100*TF_nette_2021/rfr]
merged[, Part_TF_rfr_2022 := 100*TF_nette_2022/rfr]





quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
#### Pour monter le nb de quantiles, s'il y a des doublons :
quantiles_unique <- unique(quantiles)
merged[, Quintile_rfr := cut(rfr, breaks = quantiles_unique, labels = 1:(length(quantiles_unique) - 1), include.lowest = TRUE)]


moy_2020 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr_2020, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_2020$annee <- 2020
moy_2021 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr_2021, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_2021$annee <- 2021
moy_2022 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr_2022, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_2022$annee <- 2022

moy_tf_rfr <- rbindlist(list(moy_2020, moy_2021, moy_2022))
moy_tf_rfr$annee <- as.factor(moy_tf_rfr$annee)
moy_tf_rfr$Quintile_rfr <- as.numeric(moy_tf_rfr$Quintile_rfr)


#### Puis graphique :
# On prépare tous les arguments 
data_loc <- copy(moy_tf_rfr)
x <- "Quintile_rfr"
y <- "V1"
fill <- 'annee'
xlabel <- "Déciles revenu fiscal de référence"
ylabel <- "Part du RFR payé sous forme de TF"
ysuffix <- "%"
filllabel <- "Année"

# L'adresse de sauvegarde du graphique
titre_save <- paste("Moyenne_TFPB_RFR_Toutes_annees", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

# Titres et sous-titres
titre_graphe <- "Montant de TF nette relativement au RFR du ménage"
sous_titre_graphe <- "Moyenne des ratios par quantile de RFR"

# Pour virer le titre si on le souhaite
if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

# On appelle la fonction
Faire_graphique_barplot_avec_fill(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)


################################################################################
# ================= SCENARIO 1 : A BAS LES RESIDENCES SECONDAIRES ! ============
################################################################################
# On reste sur 2021
Montant_TF_Tot <- sum(dt_merged_REI_2021$Montant_TF_NETTE_proratise_2021, na.rm = TRUE)
Montant_TF_res_princ <- sum(dt_merged_REI_2021[Residence_principale == T]$Montant_TF_NETTE_proratise_2021, na.rm = TRUE)
Montant_TF_res_sec <- sum(dt_merged_REI_2021[Residence_principale == F]$Montant_TF_NETTE_proratise_2021, na.rm = TRUE)

100*Montant_TF_res_sec/Montant_TF_Tot # = 50.8% donc on peut doubler la TF sur les res secondaires et la supprimer sur les res principales, et ça sera neutre sur le plan budgétaire !
table(dt_merged_REI_2021$Residence_principale)


### CHIFFRAGE + PRECIS RES PRINCIPALES ET SECONDAIRES
carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_2021$ident21 <- as.factor(dt_merged_REI_2021$ident21)
TF_men <- dt_merged_REI_2021[Residence_principale == T, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
Montant_TF_res_princ <- sum(merged$poi*2*merged$TF_nette, na.rm = TRUE)

TF_men <- dt_merged_REI_2021[Residence_principale == F, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
Montant_TF_res_sec <- sum(merged$poi*2*merged$TF_nette, na.rm = TRUE)

100*Montant_TF_res_sec/(Montant_TF_res_sec + Montant_TF_res_princ)


# moyenne de : TF/RFR, par quantile de RFR, observé
# Scénarion actuel
carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_2021$ident21 <- as.factor(dt_merged_REI_2021$ident21)
TF_men <- dt_merged_REI_suivi[, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
#### Pour monter le nb de quantiles, s'il y a des doublons :
quantiles_unique <- unique(quantiles)
merged[, Quintile_rfr := cut(rfr, breaks = quantiles_unique, labels = 1:(length(quantiles_unique) - 1), include.lowest = TRUE)]


moy_tf_rfr <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr$Quintile_rfr <- as.numeric(moy_tf_rfr$Quintile_rfr)

moy_tf_rfr$scenario <- "Actuel"


# Contrefactuel 1 : on double uniquement les TF des non résidences principales
dt_merged_REI_contrefact <- copy(dt_merged_REI_2021)

dt_merged_REI_contrefact[Residence_principale == F, Montant_TF_NETTE_proratise_2021 := Montant_TF_NETTE_proratise_2021*2]

carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_contrefact$ident21 <- as.factor(dt_merged_REI_contrefact$ident21)
TF_men <- dt_merged_REI_contrefact[, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
#### Pour monter le nb de quantiles, s'il y a des doublons :
quantiles_unique <- unique(quantiles)
merged[, Quintile_rfr := cut(rfr, breaks = quantiles_unique, labels = 1:(length(quantiles_unique) - 1), include.lowest = TRUE)]

moy_tf_rfr_contrefact_1 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr_contrefact_1$Quintile_rfr <- as.numeric(moy_tf_rfr_contrefact_1$Quintile_rfr)
moy_tf_rfr_contrefact_1$scenario <- "Doublée pour les RS"


# Contrefactuel 2 : on supprime uniquement les TF des résidences principales
dt_merged_REI_contrefact <- copy(dt_merged_REI_2021)

dt_merged_REI_contrefact[Residence_principale == T, Montant_TF_NETTE_proratise_2021 := 0]

carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_contrefact$ident21 <- as.factor(dt_merged_REI_contrefact$ident21)
TF_men <- dt_merged_REI_contrefact[, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
#### Pour monter le nb de quantiles, s'il y a des doublons :
quantiles_unique <- unique(quantiles)
merged[, Quintile_rfr := cut(rfr, breaks = quantiles_unique, labels = 1:(length(quantiles_unique) - 1), include.lowest = TRUE)]

moy_tf_rfr_contrefact_2 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr_contrefact_2$Quintile_rfr <- as.numeric(moy_tf_rfr_contrefact_2$Quintile_rfr)
moy_tf_rfr_contrefact_2$scenario <- "Supprimée pour les RP"



# Contrefactuel 3 : combinaison des deux
dt_merged_REI_contrefact <- copy(dt_merged_REI_2021)

ratio <- Montant_TF_res_princ/Montant_TF_Tot # Les résidences principales représentent 49% de la TF totale ==> On peut doubler la TF des res secondaires et enlever la TF des res principales !

dt_merged_REI_contrefact[Residence_principale == F, Montant_TF_NETTE_proratise_2021 := Montant_TF_NETTE_proratise_2021*2]
dt_merged_REI_contrefact[Residence_principale == T, Montant_TF_NETTE_proratise_2021 := 0]
carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_contrefact$ident21 <- as.factor(dt_merged_REI_contrefact$ident21)
TF_men <- dt_merged_REI_contrefact[, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
#### Pour monter le nb de quantiles, s'il y a des doublons :
quantiles_unique <- unique(quantiles)
merged[, Quintile_rfr := cut(rfr, breaks = quantiles_unique, labels = 1:(length(quantiles_unique) - 1), include.lowest = TRUE)]

moy_tf_rfr_contrefact_3 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr_contrefact_3$Quintile_rfr <- as.numeric(moy_tf_rfr_contrefact_2$Quintile_rfr)
moy_tf_rfr_contrefact_3$scenario <- "Combinaison des deux"

## Chiffrage montant tot :
sum(merged$poi*2*merged$TF_nette, na.rm = TRUE)


moy_tf_rfr <- rbindlist(list(moy_tf_rfr, moy_tf_rfr_contrefact_1, moy_tf_rfr_contrefact_2, moy_tf_rfr_contrefact_3))


#### Puis graphique :
data_loc <- copy(moy_tf_rfr)
x <- "Quintile_rfr"
y <- "V1"
fill <- 'scenario'
xlabel <- "Vingtiles de revenu fiscal de référence"
ylabel <- "Part du RFR payé sous forme de TF"
ysuffix <- "%"
filllabel <- "Scénario :"

titre_save <- paste("Moyenne_TFPB_RFR_Toutes_annees_scenar_1", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

titre_graphe <- "Montant de TF nette relativement au RFR du ménage dans le scénario 1"
sous_titre_graphe <- "Moyenne des ratios par quantile de RFR"

if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}
Faire_graphique_barplot_avec_fill(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix, titre_save, titre_graphe, sous_titre_graphe)

####################################################################################################
# ================= SCENARIO 2 : BAISSE DE 50% DE L'ASSIETTE TAXEE POUR LES RESIDENCES 
#                                 PRINCIPALES SITUEES HORS D'UNE UNITE URBAINE
####################################################################################################

# On simule une assiette taxable de TFPB qui passerait de VLC/2 à VLC/4 pour les
# communes hors unité urbaine (proxy des communes rurales)

# On part de la TFPB 2021 et on calcule une TFPB brute post-réforme :
# Sont éligibles à la réforme, les résidences principales situées hors UU.
dt_merged_REI_2021_loc<-copy(dt_merged_REI_2021)
dt_merged_REI_2021_loc[, Montant_TF_BRUT2 := Montant_TF_BRUT/2]
dt_merged_REI_2021_loc[ , Montant_TF_BRUT_postref := data.table::fcase(TYPE_COMMUNE_UU == "Hors unité urbaine" & Residence_principale ==T, Montant_TF_BRUT2,
                                                                       TYPE_COMMUNE_UU != "Hors unité urbaine" | Residence_principale ==F, Montant_TF_BRUT)
]


# On calcule l'effet de la réforme en faisant la simple différence entre Montant_TF_BRUT_postref
# et Montant_TF_BRUT:
dt_merged_REI_2021_loc[, effet_reforme := Montant_TF_BRUT_postref-Montant_TF_BRUT]

# On calcule l'effet moyen par département:
effet_reforme_scenario2_par_dep <-dt_merged_REI_2021_loc[, mean(effet_reforme,na.rm=T), by = "ccodep"]

# On fait une carte des baisses moyennes de TFPB par département: voir ci-dessous
# dans la rubrique "CARTE PAR DEPARTEMENT".

# Pour compenser la perte de recettes fiscales, nous pourrions imaginer une taxation
# des résidences secondaires non pas sur la base de VLC/2 mais de VLC...


################################################################################
############ CARTE PAR DEPARTEMENT #############################################
################################################################################

# Moyenne de TF nette payée par département
#dt_mean_dep <- dt_merged_REI_2021[, mean(Montant_TF_NETTE_proratise_2021), by = "ccodep"]
#setnames(dt_mean_dep, "V1", "Fill_carte")
# Forme attendue : dt_mean_dep = 1 colonne ccodep + 1 colonne Fill_carte

# Taux apparent moyen de TF nette payée par département
dt_tx_apparent_dep <- dt_merged_REI_2021[, .(TF_nette = sum(Montant_TF_NETTE_proratise_2021,na.rm=T),
                                             ASSIETTE_TF = sum(bipeva,na.rm = T)), by = "ccodep"
                                             ][,
                                               tx_apparent_TF_nette := TF_nette/ASSIETTE_TF*100
                                               ][,.(ccodep,tx_apparent_TF_nette)]
setnames(dt_tx_apparent_dep, "tx_apparent_TF_nette", "Fill_carte")


# Importation fond de carte
dep <- st_read(paste(repo_data, "dep_franceentiere_2022.gpkg", sep = "/"), quiet = TRUE)

# Merge sur le code département
dt_merged <- merge(dep, dt_tx_apparent_dep, by.x = "code", by.y = "ccodep")

# Puis le graphique
data_loc <- copy(dt_merged)
titre_graphe <- "Carte du taux apparent moyen de TFNB nette par département en 2021"
sous_titre_graphe <- "En %"
filllabel <- "En %"

if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

titre_save <- paste("Carte_tx_apparent_moyen_dep", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

Faire_carte_departements(data_loc, filllabel, titre_graphe, sous_titre_graphe, titre_save)



# Baisse moyenne de TFPB brute induite par la réforme scenario 2 par département:

setnames(effet_reforme_scenario2_par_dep, "V1", "Fill_carte")

# Importation fond de carte
dep <- st_read(paste(repo_data, "dep_franceentiere_2022.gpkg", sep = "/"), quiet = TRUE)

# Merge sur le code département
dt_merged <- merge(dep, effet_reforme_scenario2_par_dep, by.x = "code", by.y = "ccodep")

# Puis le graphique
data_loc <- copy(dt_merged)
titre_graphe <- "Baisse moyenne de TFPB brute induite par la réforme scenario 2 par département"
#sous_titre_graphe <- "En euros"
filllabel <- "En euros"

if(!mettre_titres_graphiques){
  titre_graphe <- ""
  sous_titre_graphe <- ""
}

titre_save <- paste("Carte_effet_moy_scenario2_dep", ".pdf", sep = "") 
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins_graphes <- append(liste_chemins_graphes, titre_save)

Faire_carte_departements(data_loc, filllabel, titre_graphe, sous_titre_graphe, titre_save)


################################################################################
########## FUSION CAHIER GRAPHIQUE #############################################
################################################################################
# A garder à la fin du script : récupère tous les graphiques pdf générés et les fusionne dans le dossier racine du projet

titre_save <- paste("cahier_graphique_",annee,".pdf", sep = "")
titre_save <- paste(repgen, titre_save, sep ='/')
pdf_combine(liste_chemins_graphes, output = titre_save)



################################################################################
############################# STAT DES SUR LA BASE DE DONNEES ##################
################################################################################


table(dt_merged_REI$Raison_exoneration)
nrow(dt_merged_REI[Montant_TF_NETTE != Montant_TF_BRUT])
nrow(dt_merged_REI)


dt_merged_REI[, .N, by = 'ccodep'][order(N)]
nrow(carac_men)

# Distribution nivviem
N_tot <- sum(carac_men$poi)
print(xtable(carac_men[, 100*sum(poi)/N_tot, by = "decile_ndv"]), include.rownames = FALSE)


# Distribution RFR
dw <- svydesign(ids = ~1, data =carac_men, weights = ~ carac_men$poi)
tab <- svyquantile(~ rfr, dw, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
tab <- as.data.table(tab$rfr)
setnames(tab, "quantile", "rfr")
moy <- as.data.table(svymean(~ rfr, na.rm = TRUE, dw))$mean

tab$quantile <- c(0.1, 0.25, 0.5, 0.75, 0.9)
dt_mean <- data.table(quantile = "Moyenne", rfr = moy)

l <- c("quantile", "rfr")

print(xtable(rbindlist(list(tab[,..l], dt_mean))), include.rownames = FALSE)


sum(carac_men[rfr < rfr_min]$poi)/sum(carac_men$poi)


# Quelques chiffres sur la TFPB 2021
dt_merged_REI$ident21 <- as.factor(dt_merged_REI$ident21)
dt_merged_REI_2021$ident21 <- as.factor(dt_merged_REI_2021$ident21)
dt_merged_REI_2020$ident21 <- as.factor(dt_merged_REI_2020$ident21)
dt_merged_REI_2022$ident21 <- as.factor(dt_merged_REI_2022$ident21)
carac_men$ident <- as.factor(carac_men$ident)


TF_men <- dt_merged_REI_2021[, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
sum(merged$poi*2*merged$V1, na.rm = TRUE) # Le total

TF_men <- dt_merged_REI_2021[, sum(Montant_TF_NETTE_proratise_2021,na.rm = T), by = "ident21"]
merged <- merge(TF_men, carac_men, all.y = TRUE, by.x = "ident21", by.y = "ident")
merged[, weighted.mean(V1, w=poi, na.rm = TRUE)]

sum(carac_men$poi*2)

# Comparaison entre années
TF_men <- dt_merged_REI_2022[, sum(Montant_TF_BRUT_proratise_2022,na.rm = T), by = "ident21"]
merged <- merge(TF_men, carac_men, all.y = TRUE, by.x = "ident21", by.y = "ident")
sum(merged$poi*2*merged$V1, na.rm = TRUE) # Le total
merged[, weighted.mean(V1, w=poi, na.rm = TRUE)]



################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################












