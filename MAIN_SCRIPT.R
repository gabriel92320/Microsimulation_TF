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

repgen <- "C:/Users/Benjamin/Desktop/Ensae/3A-M2/Eco_redistribution"
 # repgen <- "/Users/gabrielsklenard/Documents/Memoire_Microsimulation"


repo_prgm <- paste(repgen, "MicrosimulationTF" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_bases_intermediaires <- paste(repgen, "Bases_intermediaires" , sep = "/")

# Ici quelques paramètres généraux
annee <- 2021 # Pour 2020 il y a le tx départemental en plus
toutes_annees <- TRUE # Pour calculer les montants sur les 3 années 2020, 2021 et 2022

mettre_titres_graphiques <- TRUE # Pour sauvegarder les graphes SANS leur titre (pour pouvoir mettre le titre en caption latex)
utiliser_dvldif2a <- TRUE # Pour utiliser la variable dvldif2a = Montant de VL exonérée (valeur de l’année) pour le calcul TF net ==> Ne change pas grand chose, mais je pense que c'est plus propre parce que déjà contenu dans la base

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

summary(carac_men$rfr)
nrow(carac_men[rfr < 100])

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
# quantiles_unique <- unique(quantiles)
merged[, Quintile_rfr := cut(rfr, breaks = quantiles, labels = 1:(length(quantiles) - 1), include.lowest = TRUE)]


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


# moyenne de : TF/RFR, par quantile de RFR, observé
# Scénarion actuel
carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_2021$ident21 <- as.factor(dt_merged_REI_2021$ident21)
TF_men <- dt_merged_REI_suivi[, sum(Montant_TF_NETTE_proratise_2021), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
merged[, Quintile_rfr := cut(rfr, breaks = quantiles, labels = 1:(length(quantiles) - 1), include.lowest = TRUE)]

moy_tf_rfr <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr$Quintile_rfr <- as.numeric(moy_tf_rfr$Quintile_rfr)

moy_tf_rfr$scenario <- "Actuel"


# Contrefactuel 1 : on double uniquement les TF des résidences secondaires
dt_merged_REI_contrefact <- copy(dt_merged_REI_2021)

dt_merged_REI_contrefact[Residence_principale == F, Montant_TF_NETTE_proratise_2021 := Montant_TF_NETTE_proratise_2021*2]

carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_contrefact$ident21 <- as.factor(dt_merged_REI_contrefact$ident21)
TF_men <- dt_merged_REI_contrefact[, sum(Montant_TF_NETTE_proratise_2021), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
merged[, Quintile_rfr := cut(rfr, breaks = quantiles, labels = 1:(length(quantiles) - 1), include.lowest = TRUE)]

moy_tf_rfr_contrefact_1 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr_contrefact_1$Quintile_rfr <- as.numeric(moy_tf_rfr_contrefact_1$Quintile_rfr)
moy_tf_rfr_contrefact_1$scenario <- "TF doublée pour les RS"


# Contrefactuel 2 : on supprime uniquement les TF des résidences principales
dt_merged_REI_contrefact <- copy(dt_merged_REI_2021)

dt_merged_REI_contrefact[Residence_principale == T, Montant_TF_NETTE_proratise_2021 := 0]

carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_contrefact$ident21 <- as.factor(dt_merged_REI_contrefact$ident21)
TF_men <- dt_merged_REI_contrefact[, sum(Montant_TF_NETTE_proratise_2021), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
merged[, Quintile_rfr := cut(rfr, breaks = quantiles, labels = 1:(length(quantiles) - 1), include.lowest = TRUE)]

moy_tf_rfr_contrefact_2 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr_contrefact_2$Quintile_rfr <- as.numeric(moy_tf_rfr_contrefact_2$Quintile_rfr)
moy_tf_rfr_contrefact_2$scenario <- "TF supprimée pour les RP"



# Contrefactuel 3 : combinaison des deux
dt_merged_REI_contrefact <- copy(dt_merged_REI_2021)

ratio <- Montant_TF_res_princ/Montant_TF_Tot # Les résidences principales représentent 49% de la TF totale ==> On peut doubler la TF des res secondaires et enlever la TF des res principales !

dt_merged_REI_contrefact[Residence_principale == F, Montant_TF_NETTE_proratise_2021 := Montant_TF_NETTE_proratise_2021*2]
dt_merged_REI_contrefact[Residence_principale == T, Montant_TF_NETTE_proratise_2021 := 0]
carac_men$ident <- as.factor(carac_men$ident)
dt_merged_REI_contrefact$ident21 <- as.factor(dt_merged_REI_contrefact$ident21)
TF_men <- dt_merged_REI_contrefact[, sum(Montant_TF_NETTE_proratise_2021), by = "ident21"]
setnames(TF_men, "V1", "TF_nette")
merged <- merge(TF_men, carac_men, all.x = TRUE, by.x = "ident21", by.y = "ident")
merged[, Part_TF_rfr := 100*TF_nette/rfr]

quantiles <- weighted_quantiles(merged$rfr, merged$poi, probs = seq(0, 1, length.out = nb_quantiles + 1))
merged[, Quintile_rfr := cut(rfr, breaks = quantiles, labels = 1:(length(quantiles) - 1), include.lowest = TRUE)]

moy_tf_rfr_contrefact_3 <- merged[rfr > rfr_min, weighted.mean(Part_TF_rfr, w=poi, na.rm = TRUE), by = "Quintile_rfr"]
moy_tf_rfr_contrefact_3$Quintile_rfr <- as.numeric(moy_tf_rfr_contrefact_2$Quintile_rfr)
moy_tf_rfr_contrefact_3$scenario <- "Combinaison des deux"





moy_tf_rfr <- rbindlist(list(moy_tf_rfr, moy_tf_rfr_contrefact_1, moy_tf_rfr_contrefact_2, moy_tf_rfr_contrefact_3))

#### Puis graphique :
data_loc <- copy(moy_tf_rfr)
x <- "Quintile_rfr"
y <- "V1"
fill <- 'scenario'
xlabel <- "Déciles revenu fiscal de référence"
ylabel <- "Part du RFR payé sous forme de TF"
ysuffix <- "%"
filllabel <- "Scénario"

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




################################################################################
########## FUSION CAHIER GRAPHIQUE #############################################
################################################################################
# A garder à la fin du script : récupère tous les graphiques pdf générés et les fusionne dans le dossier racine du projet

titre_save <- paste("cahier_graphique_",annee,".pdf", sep = "")
titre_save <- paste(repgen, titre_save, sep ='/')
pdf_combine(liste_chemins_graphes, output = titre_save)

################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################












################################################################################
################### BROUILLON GABRIEL ########################################## 
################################################################################

# Copie préalable de la base d'étude:
dt_merged_REI2 <- dt_merged_REI

# Importer carac_men et carac_tf
carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))
carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))

# 1) Calcul du montant de la taxe foncière sur les propriétés bâties (TFPB) brute:

dt_merged_REI2$bipeva <- as.numeric(dt_merged_REI2$bipeva)

# a) Calcul de la TF brute pour chaque propriété bâtie 
# (en distinguant la partie communale et la partie EPCI):
dt_merged_REI2[, Montant_communal_TF := FB_COMMUNE_TAUX_NET * bipeva/100]
dt_merged_REI2[, Montant_GFP_TF := FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE * bipeva/100]
dt_merged_REI2[, Montant_TF_BRUT := Montant_communal_TF + Montant_GFP_TF]

# b) Calcul de la TF brute pour chaque ménage:

TFPB_brute_menages <- dt_merged_REI2[, .(TFPB_brute = sum(Montant_TF_BRUT,na.rm = T)), by = 'ident21']

# c) Merge avec la base des caractéristiques "ménage":
TFPB_brute_menages <- merge(carac_men, TFPB_brute_menages, by.x = "ident", by.y = "ident21")

# 2) Quelques statistiques descriptives sur la TFPB brute en 2021 en fonction
# de qques caractéristiques des ménages propriétaires:

# Passage en tibble pour faire du Tidyverse:
TFPB_brute_menages_tb <- as_tibble(TFPB_brute_menages)
colnames(TFPB_brute_menages_tb)
str(TFPB_brute_menages_tb)

# Nombre de ménages imposés à la TF en 2021 en France (avant exonération),en millions:
print((sum(TFPB_brute_menages$poi)*2)/1e6)
# 16,8 millions.

# Montant total de TFPB brute 2021 (en milliard d'euros):
print(sum(TFPB_brute_menages$TFPB_brute)/1e9)
# 0,02 Md euros;

# Somme pondérée par les poids de sondage de ERFS*2:
print(sum(TFPB_brute_menages$TFPB_brute*TFPB_brute_menages$poi*2)/1e9)
# 29,6 Md euros (on semble être dans les mêmes ordres de grandeur que ce
# qui est publié par la DGFiP pour 2022, cf. Soulignac, 2023)

# Montant moyen de TFPB 2021, en euros:
print(sum(TFPB_brute_menages$TFPB_brute*TFPB_brute_menages$poi*2)/(sum(TFPB_brute_menages$poi)*2))
# 1 761 euros pour un ménage propriétaire en 2021.

#stat <- t %>%
#  group_by(categorie) %>%
#  summarise(mediane = wtd.quantile(y, weights = p, probs = 0.5, type = 'quantile'))

# Taux moyen de TFPB brute par décile de niveau de vie en 2021:

TFPB_brute_menages_tb <- TFPB_brute_menages_tb %>%
                         filter(!(is.na(decile_ndv))) %>%
                         mutate(decile_ndv = factor(decile_ndv)) %>%
                         mutate(decile_ndv=fct_recode(decile_ndv,"D1"="1","D2"="2","D3"="3","D4"="4",
                                    "D5"="5","D6"="6","D7"="7","D8"="8","D9"="9",
                                    "D10"="10"))

# Calculs sans pondération:
TFPB_brute_decile_ndv_np <- TFPB_brute_menages_tb %>%
                         group_by(decile_ndv) %>%
                         summarise(Nb_menages = n(),
                                   TFPB_brute_tot = sum(TFPB_brute,na.rm = T),
                                   TFPB_brute_moy = mean(TFPB_brute,na.rm = T))

# Calculs avec pondération:
TFPB_brute_decile_ndv_p <- TFPB_brute_menages_tb %>%
  mutate(poi2=2*poi) %>%
  group_by(decile_ndv) %>%
  summarise(Nb_menages = sum(poi2,na.rm = T),
    TFPB_brute_tot = sum(TFPB_brute*poi2,na.rm = T)
            ) %>%
  mutate(TFPB_brute_moy = TFPB_brute_tot/Nb_menages) %>%
  mutate(Nb_menages2 = Nb_menages/1e6, #en millions de ménages
         TFPB_brute_tot2 = TFPB_brute_tot/1e9 #en milliard d'euros
  ) %>%
  select(decile_ndv,Nb_menages2,TFPB_brute_tot2,TFPB_brute_moy) %>%
  rename(Nb_menages=Nb_menages2,TFPB_brute_tot=TFPB_brute_tot2
         )

# Visualisation graphique: 

# Graphique 1:
#TFPB_brute totale (en Md euros) par décile de niveau de vie des ménages en 2021

graph1 <- ggplot(TFPB_brute_decile_ndv_p) +
  aes(x = decile_ndv, y = TFPB_brute_tot) +
  geom_col(fill = "#112446") +
  labs(
    x = "Déciles de niveau de vie",
    y = "TFPB (en milliard d'euros)",
    title = "Montant total de TFPB brute en 2021",
    subtitle = "Distribution selon le niveau de vie des ménages propriétaires"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#ggsave("graph1.pdf",plot = graph1,path = repo_sorties)

# Graphique 2:
#TFPB_brute moyenne (en euros) par décile de niveau de vie des ménages en 2021

graph2 <- ggplot(TFPB_brute_decile_ndv_p) +
  aes(x = decile_ndv, y = TFPB_brute_moy) +
  geom_col(fill = "#112446") +
  labs(
    x = "Déciles de niveau de vie",
    y = "TFPB (en euros)",
    title = "Montant moyen de TFPB brute en 2021",
    subtitle = "Distribution selon le niveau de vie des ménages propriétaires"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#ggsave("graph2.pdf",plot = graph2,path = repo_sorties)

# Calcul du taux moyen apparent de TFPB par décile de niveau de vie en 2021:

# Récupération de l'assiette de la TFPB par ménage
assiette_TFPB_brute_tb <- carac_tf %>%
               as_tibble() %>%
               select(ident21,bipeva) %>%
               mutate(bipeva=as.numeric(bipeva)) %>%
               group_by(ident21) %>%
               summarise(assiette_TFPB=sum(bipeva,na.rm = T)) 

# Jointure avec les caractéristiques des ménages + TFPB brute simulée 2021
TFPB_brute_menages_tb <- TFPB_brute_menages_tb %>%
                         left_join(assiette_TFPB_brute_tb,
                                   by=c("ident"="ident21"))

# Calcul du taux moyen apparent de TFPB par ménage
TFPB_brute_menages_tb <- TFPB_brute_menages_tb %>%
                         mutate(tx_apparent_TFPB_brute=TFPB_brute/assiette_TFPB*100,
                                poi2 = poi*2)
      

# Visualisation de la distribution des taux apparents par décile de niveau de vie:
# calcul des fractiles sans pondérer:
graph3a <-ggplot(TFPB_brute_menages_tb) +
  aes(x = decile_ndv, y = tx_apparent_TFPB_brute) +
  geom_boxplot(fill = "#112446") +
  labs(
    x = "Déciles de niveau de vie",
    y = "Taux apparent (en %)",
    title = "Taux apparent de TFPB brute en 2021",
    subtitle = "Distribution au sein de chaque décile de niveau de vie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# calcul des fractiles en pondérant:
graph3b <-ggplot(TFPB_brute_menages_tb) +
  aes(x = decile_ndv, y = tx_apparent_TFPB_brute,weight=poi2) +
  geom_boxplot(fill = "#112446") +
  labs(
    x = "Déciles de niveau de vie",
    y = "Taux apparent (en %)",
    title = "Taux apparent de TFPB brute en 2021",
    subtitle = "Distribution au sein de chaque décile de niveau de vie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Calcul du taux moyen apparent par décile niveau de vie:

tx_moyen_apparent_decile_ndv <- TFPB_brute_menages_tb %>%
                                group_by(decile_ndv) %>%
                                summarise(TFPB_brute=sum(TFPB_brute*poi2,na.rm = T),
                                          assiette_TFPB = sum(assiette_TFPB*poi2,na.rm = T)) %>%
                                mutate(tx_apparent_moy_TFPB_b=TFPB_brute/assiette_TFPB*100) %>%
                                mutate(assiette_TFPB_mde=assiette_TFPB/1e9)

# Visualisation du taux moyen apparent de TFPB brute en 2021 par décile de
# niveau de vie des ménages:
graph4 <- ggplot(tx_moyen_apparent_decile_ndv) +
  aes(x = decile_ndv, y = tx_apparent_moy_TFPB_b) +
  geom_col(fill = "#112446") +
  labs(
    x = "Déciles de niveau de vie",
    y = "Taux moyen apparent",
    title = "Taux moyen apparent de TFPB brute en 2021",
    subtitle = "Distribution par décile de niveau de vie des ménages"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Visualisation du montant total d'assiette de TFPB brute 2021
# par décile de niveau de vie des ménages
graph5 <- ggplot(tx_moyen_apparent_decile_ndv) +
  aes(x = decile_ndv, y = assiette_TFPB_mde) +
  geom_col(fill = "#112446") +
  labs(
    x = "Déciles de niveau de vie",
    y = "Assiette (en milliard d'euros)",
    title = "Assiette de la TFPB brute en 2021",
    subtitle = "Distribution par décile de niveau de vie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


######################################################################
# Impact des changements de taux de TFPB en 2023 par rapport à 2022:

# Récupération des taux locaux de TFPB 2022 (fichier REI 2022):
REI_22 <- data.table(read.xlsx(paste(repo_data, "REI_2022.xlsx", sep = "/")))
SOUS_REI_22 <- REI_22[, ..liste_cols_REI]
rm(REI_22)
save(SOUS_REI_22, file = paste(repo_bases_intermediaires, "REI_2022_SELECT.RData", sep = "/"))

# Importe et merge : carac_men, carac_tf et SOUS_REI. On ne garde que quelques colonnes de SOUS_REI
# Retourne le dt merged
# ATTENTION SI ON AJOUTE DES COLONNES IL VA FALLOIR SANS DOUTE AJOUTER DES SETNAMES
  
  # Importer le REI et sélectionner les colonnes
  load(paste(repo_bases_intermediaires, "REI_2022_SELECT.RData", sep = "/"))
  SOUS_REI <- SOUS_REI_22[,..liste_cols_REI_loc]
  
  # Faire quelques setnames pour éviter les problèmes
  try(setnames(SOUS_REI, "Numéro.national.du.groupement", "Numero_national_du_groupement"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.COMMUNE./.TAUX.NET", "FB_COMMUNE_TAUX_NET"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", "FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.TSE./.TAUX.NET", "FB_TSE_TAUX_NET"), silent = TRUE)
  try(setnames(SOUS_REI, "Libellé.commune", "Libelle_commune"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.GFP./.TAUX.VOTE", "FB_GFP_TAUX_VOTE"), silent = TRUE)
  
  
  # Importer carac_men et carac_tf
  carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))
  carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))
  
  # Merge TF et MEN
  dt_merged <- merge(carac_tf, carac_men, by.x = "ident21", by.y = "ident")
  
  # Faire attention au typage
  dt_merged$ccocom_REI <- as.factor(dt_merged$ccocom_REI)
  dt_merged$ccodep <- as.factor(dt_merged$ccodep)
  SOUS_REI$DEPARTEMENT <- as.factor(SOUS_REI$DEPARTEMENT)
  SOUS_REI$COMMUNE <- as.factor(SOUS_REI$COMMUNE)
  
  # ATTENTION pour Lyon, Paris, Marseille il faut changer le ccocom pour mettre le code de la commune, et non pas des arrondissements
  liste_num_Lyon <- 381:389
  liste_num_Marseille <- 201:216
  liste_num_Paris <- 101:120
  
  dt_merged[, ccocom_REI := ccocom]
  dt_merged[ccodep == "69" & ccocom_REI %in% liste_num_Lyon, ccocom_REI := "123"] # On met le code de la commune pour avoir le REU, et pas le code des arrondissement
  dt_merged[ccodep == "13" & ccocom_REI %in% liste_num_Marseille, ccocom_REI := "055"] 
  dt_merged[ccodep == "75" & ccocom_REI %in% liste_num_Paris, ccocom_REI := "056"]
  
  # Puis Merge merged et REI
  dt_merged_VLC21_REI22 <- merge(dt_merged, SOUS_REI, by.x = c("ccodep", "ccocom_REI"), by.y = c("DEPARTEMENT", "COMMUNE"), all.x = TRUE)
  
# Calcul de la TFPB à partir de l'assiette 2021 et des taux 2022:

  dt_merged_VLC21_REI22$bipeva <- as.numeric(dt_merged_VLC21_REI22$bipeva)
  
  # a) Calcul de la TF brute pour chaque propriété bâtie 
  # (en distinguant la partie communale et la partie EPCI):
  dt_merged_VLC21_REI22[, Montant_communal_TF := FB_COMMUNE_TAUX_NET * bipeva/100]
  dt_merged_VLC21_REI22[, Montant_GFP_TF := FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE * bipeva/100]
  dt_merged_VLC21_REI22[, Montant_TF_BRUT := Montant_communal_TF + Montant_GFP_TF]
  
  # b) Calcul de la TF brute pour chaque ménage:
  
  TFPB_brute_menages2 <- dt_merged_VLC21_REI22[, .(TFPB_brute2 = sum(Montant_TF_BRUT,na.rm = T)), by = 'ident21']
  
# Passage en tibble et merge avec la base TFPB_brute_menages_tb (calcul de la 
# TFPB 2021)
  TFPB_brute_menages2_tb <- as_tibble(TFPB_brute_menages2)
  TFPB_brute_menages_tb <- left_join(TFPB_brute_menages_tb,TFPB_brute_menages2_tb,by=c("ident"="ident21"))
  
# Calcul de l'effet "évolution des taux entre 2021 et 2022" pour chaque ménage:
  TFPB_brute_menages_tb <- TFPB_brute_menages_tb %>%
                          mutate(effet_evol_tx_21_22=TFPB_brute2-TFPB_brute)

# Calcul de l'effet agrégé et moyen de "l'évolution des taux entre 2021 et 2022" par décile
# de niveau de vie:
  effet_evol_tx_decile_ndv <- TFPB_brute_menages_tb %>%
  group_by(decile_ndv) %>%
    summarise(nb_ménages=sum(poi2),
      effet_evol_tx_21_22 = sum(effet_evol_tx_21_22*poi2,na.rm = T)
    ) %>%
    mutate(effet_moy_evol_tx_21_22=effet_evol_tx_21_22/nb_ménages) %>%
    mutate(effet_evol_tx_21_22_2=effet_evol_tx_21_22/1e9)
    
# Visualisation de l'effet agrégé "évolution des taux 21-22" par
# décile de niveau de vie:

  ggplot(effet_evol_tx_decile_ndv) +
    aes(x = decile_ndv, y = effet_evol_tx_21_22_2) +
    geom_col(fill = "#112446") +
    labs(
      x = "Déciles de niveau de vie",
      y = "En milliard d'euros",
      title = "Effet de l'évolution des taux de TFPB entre 2021 et 2022",
      subtitle = "Distribution de l'effet agrégé par décile de niveaux de vie"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
# Visualisation de l'effet moyen "évolution des taux 21-22" par
# décile de niveau de vie
  ggplot(effet_evol_tx_decile_ndv) +
    aes(x = decile_ndv, y = effet_moy_evol_tx_21_22) +
    geom_col(fill = "#112446") +
    labs(
      x = "Déciles de niveau de vie",
      y = "En euros",
      title = "Effet de l'évolution des taux de TFPB entre 2021 et 2022",
      subtitle = "Effet moyen par décile de niveau de vie"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold",
                                hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )


# Regarder l'effet "évolution des taux 21-22" sous d'autres angles:
# classe d'âge, par région, par type d'unité urbaine ?

# TODO:
# Récupérer les zonages TUU 2020 et les apparier à la base "logements"
# Coder une variable catégorielle de classe d'âge


    
  
######################################################################
# Implémentation des exonérations de TFPB:

# TODO!

# Distinguer exonération totale/exonération partielle








