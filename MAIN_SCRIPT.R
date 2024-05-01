################################################################################
########### FICHIER MAIN POUR LA MICROSIMULATION DE LA TF ######################
################################################################################

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



################################################################################
# =========== 01 = PACKAGES ET SCRIPTS DE FONCTIONS  ===========================
################################################################################
source(paste(repo_prgm , "01_packages.R" , sep = "/"))
source(paste(repo_prgm , "02_Importations_preparation.R" , sep = "/"))
source(paste(repo_prgm , "03_Calcul_taux_TF.R" , sep = "/"))

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
                       "FB.-.GFP./.TAUX.VOTE", # Le taux pour le montant brut à l'échelle du GFP = EPCI
                       "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", # Le tx pour montant net
                       "Libellé.commune")


# On merge REI et carac_tf, on est donc à l'échelle du LOGEMENT
dt_merged_REI <- Importer_et_merge_REI_carac_tf(liste_cols_REI_loc)

# On calcule la TF brute par logement
dt_merged_REI_loc <- copy(dt_merged_REI)
dt_merged_REI <- Calculer_taux_brut(dt_merged_REI_loc)
  
# On importe aussi carac_men
carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))

# On calcule la TF nette par logement
carac_men_loc <- copy(carac_men)
dt_merged_REI_loc <- copy(dt_merged_REI)
dt_merged_REI <- Calculer_taux_net(dt_merged_REI_loc, carac_men_loc)
  


################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################

##### QQ VERIFS ##############
# Un premier calcul de la taxe foncière brute
summary(dt_merged_REI$vlbaia - 2*dt_merged_REI$bipeva) # bipeva = 1/2 de la VLC ==> Ce qu'on prend comme valeur de référence pour l'impôt

dt_merged_REI[is.na(Libelle_commune)] # Bon on n'a pas les outre mer...
100*nrow(dt_merged_REI[is.na(Libelle_commune)])/nrow(dt_merged_REI) # Bon ça fait 0.21% des observations pas très grave sans doute

table(dt_merged_REI$Logement_degreve)
table(dt_merged_REI$Logement_exonere)

summary(dt_merged_REI$Montant_TF_BRUT)
summary(dt_merged_REI$Montant_TF_NETTE)

table(dt_merged_REI$Montant_TF_BRUT > dt_merged_REI$Montant_TF_NETTE) # 762 ménages ont une diminution de la TF


# Logements en double ??????
carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))
carac_tf[ident21 == "21038699"] # Hum hum il y a un logement en double là...

# En fait il y en a beaucoup !!!
nrow(unique(carac_tf))
nrow(carac_tf)





nrow(dt_merged_REI[Montant_TF_NETTE < 0]) # 8 lignes ont un tx négatif
# Pour vérifier les conditions ==> Ce sont des logements dégrévés
merge(dt_merged_REI[Montant_TF_NETTE < 0], carac_men, all.x = TRUE, all.y = FALSE, by.x = "ident21", by.y = "ident")




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








