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
                       "FB.-.GFP./.TAUX.VOTE", # Le taux pour le montant brut à l'échelle du GFP = EPCI
                       "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", # Le tx pour montant net
                       "Libellé.commune")

dt_merged_REI <- Importer_et_merge_DT_REI(liste_cols_REI_loc)

dt_merged_REI[is.na(Libelle_commune)] # Bon on n'a pas les outre mer...
100*nrow(dt_merged_REI[is.na(Libelle_commune)])/nrow(dt_merged_REI) # Bon ça fait 0.21% des observations pas très grave sans doute



################################################################################
################### BROUILLON BENJAMIN ######################################### 
################################################################################

# Fixage des types
dt_merged_REI$vlbaia <- as.numeric(dt_merged_REI$vlbaia)
dt_merged_REI$bipeva <- as.numeric(dt_merged_REI$bipeva)
dt_merged_REI$bateom <- as.numeric(dt_merged_REI$bateom)
dt_merged_REI$mvltieomx <- as.numeric(dt_merged_REI$mvltieomx)
dt_merged_REI$baomec <- as.numeric(dt_merged_REI$baomec)


# Un premier calcul de la taxe foncière brute
summary(dt_merged_REI$vlbaia - 2*dt_merged_REI$bipeva) # bipeva = 1/2 de la VLC ==> Ce qu'on prend comme valeur de référence pour l'impôt

dt_merged_REI[, Montant_communal_TF := FB_COMMUNE_TAUX_NET * bipeva/100]
dt_merged_REI[, Montant_GFP_TF := FB_GFP_TAUX_VOTE * bipeva/100]
dt_merged_REI[, Montant_TF_BRUT := Montant_communal_TF + Montant_GFP_TF]

summary(dt_merged_REI$Montant_TF_BRUT)
summary(dt_merged_REI$FB_COMMUNE_TAUX_NET)


dt_merged_REI[, Montant_TF_BRUT_menage := sum(Montant_TF_BRUT, na.rm = TRUE), by = 'ident21'] # On calcule la taxe totale payée par le ménage



data_loc <- dt_merged_REI[, mean(Montant_TF_BRUT_menage, na.rm = TRUE), by = "decile_ndv"]
x <- "decile_ndv"
y <- "V1"
xlabel <- "Décile de niveau de vie du ménage"
ylabel <- "Montant de la taxe foncière brute moyenne "
titre <- "Montant moyen de la taxe foncière brute payée par les ménages, en fonction du décile de niveau de vie"

ggplot(data = data_loc, aes(x = .data[[x]], y = .data[[y]])) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title=titre,
       x= xlabel,
       y= ylabel) + 
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "",
    suffix = "€",
    big.mark = " ",
    decimal.mark = ",")) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        text = element_text(size = 25))





################################################################################
################### BROUILLON GABRIEL ########################################## 
################################################################################






