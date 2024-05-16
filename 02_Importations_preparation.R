################################################################################
########### LES FONCTIONS POUR PREPARER LES DT EXPLOITABLES ####################
################################################################################


Faire_dt_SOUS_REI <- function(liste_cols_REI_loc, annee_loc = 2021){
  # Le fichier REI est trop volumineux, pour travailler dessus on va ne sélectionner que quelques colonnes
  # Cette fonction ne renvoie rien, mais sauvegarde REI_2021_SELECT
  
  REI_21 <- data.table(read.xlsx(paste(repo_data, "/REI_",annee_loc,".xlsx", sep = "")))
  if(annee_loc == 2020){
    REI_21 <- Renomer_colonnes_REI_2020(REI_21) # Il faut renomer les colonnes
    liste_cols_REI_loc <- append(liste_cols_REI_loc, "FB.-.DEP./.TAUX.NET") # Il faut ajouter le taux départemental !
    
    SOUS_REI <- REI_21[, ..liste_cols_REI_loc]
    SOUS_REI$COMMUNE <- iconv(SOUS_REI$COMMUNE, from = "UTF-8", to = "ASCII", sub = "")
    SOUS_REI$DEPARTEMENT <- iconv(SOUS_REI$DEPARTEMENT, from = "UTF-8", to = "ASCII", sub = "")
    SOUS_REI$Libellé.commune <- iconv(SOUS_REI$Libellé.commune, from = "UTF-8", to = "ASCII", sub = "")
    
  }
  
  SOUS_REI <- REI_21[, ..liste_cols_REI_loc]
  rm(REI_21)
  save(SOUS_REI, file = paste(repo_bases_intermediaires, "/REI_",annee_loc,"_SELECT.RData", sep = ""))
}

Importer_et_merge_REI_carac_tf <- function(liste_cols_REI_loc, annee_loc = 2021){
  # Importe et merge : carac_tf et SOUS_REI. On ne garde que quelques colonnes de SOUS_REI
  # Retourne le dt merged
  # ATTENTION SI ON AJOUTE DES COLONNES IL VA FALLOIR SANS DOUTE AJOUTER DES SETNAMES
  
  # Importer le REI et sélectionner les colonnes
  load(paste(repo_bases_intermediaires, "/REI_",annee_loc,"_SELECT.RData", sep = ""))
  SOUS_REI <- SOUS_REI[,..liste_cols_REI_loc]
  
  
  # Faire quelques setnames pour éviter les problèmes
  try(setnames(SOUS_REI, "Numéro.national.du.groupement", "Numero_national_du_groupement"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.COMMUNE./.TAUX.NET", "FB_COMMUNE_TAUX_NET"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE", "FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.TSE./.TAUX.NET", "FB_TSE_TAUX_NET"), silent = TRUE)
  try(setnames(SOUS_REI, "Libellé.commune", "Libelle_commune"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.GFP./.TAUX.VOTE", "FB_GFP_TAUX_VOTE"), silent = TRUE)
  try(setnames(SOUS_REI, "FB.-.DEP./.TAUX.NET", "FB_DEP_TAUX_NET"), silent = TRUE)
  

  
  # Importer carac_men et carac_tf
  # carac_men <- data.table(readRDS(paste(repo_data, "carac_men.rds", sep = "/")))
  carac_tf <- data.table(readRDS(paste(repo_data, "carac_tf.rds", sep = "/")))
  
  
  # carac_tf <- copy(unique(carac_tf)) # Pour virer les logements en double ==> EN FAIT NON ON PRORATISE APRES
  # Merge TF et MEN
  # dt_merged <- merge(carac_tf, carac_men, by.x = "ident21", by.y = "ident")
  
  
  # Convertir les colonnes en character
  carac_tf[, ccocom_REI := as.character(ccocom)]
  carac_tf[, ccodep := as.character(ccodep)]
  SOUS_REI[, DEPARTEMENT := as.character(DEPARTEMENT)]
  SOUS_REI[, COMMUNE := as.character(COMMUNE)]  
  
  
  
  extraire_numeros <- function(chaine) { # Pour l'année 2020...
    # Recherche du dernier "_" dans la chaîne
    dernier_underscore <- max(gregexpr("_", chaine)[[1]])
    # Extraction des caractères après le dernier "_"
    numeros <- substr(chaine, dernier_underscore + 1, nchar(chaine))
    return(numeros)
  }

  if(annee_loc == 2020){ # Le format est chiant
    SOUS_REI$DEPARTEMENT   <- gsub("_x0030_", "0", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0031_", "1", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0032_", "2", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0033_", "3", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0034_", "4", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0035_", "5", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0036_", "6", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0037_", "7", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0038_", "8", SOUS_REI$DEPARTEMENT  )
    SOUS_REI$DEPARTEMENT   <- gsub("_x0039_", "9", SOUS_REI$DEPARTEMENT  )
    
    SOUS_REI$COMMUNE <- gsub("_x0030_", "0", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0031_", "1", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0032_", "2", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0033_", "3", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0034_", "4", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0035_", "5", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0036_", "6", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0037_", "7", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0038_", "8", SOUS_REI$COMMUNE)
    SOUS_REI$COMMUNE <- gsub("_x0039_", "9", SOUS_REI$COMMUNE)
  }
  
  

  
  # Ajouter des zéros au début si nécessaire pour obtenir un identifiant à trois chiffres
  carac_tf[, ccocom_REI := sprintf("%03d", as.integer(ccocom_REI))]
  carac_tf[, ccoifp := sprintf("%03d", as.integer(ccoifp))]
  carac_tf[, ccocom := sprintf("%03d", as.integer(ccocom))]
  
  
  # ATTENTION pour Lyon, Paris, Marseille il faut changer le ccocom pour mettre le code de la commune, et non pas des arrondissements
  liste_num_Lyon <- as.character(381:389)
  liste_num_Marseille <- as.character(201:216)
  liste_num_Paris <- as.character(101:120)
  
  carac_tf[, ccocom_REI := ccocom]
  carac_tf[ccodep == "69" & ccocom_REI %in% liste_num_Lyon, ccocom_REI := "123"] # On met le code de la commune pour avoir le REU, et pas le code des arrondissement
  carac_tf[ccodep == "13" & ccocom_REI %in% liste_num_Marseille, ccocom_REI := "055"] 
  carac_tf[ccodep == "75" & ccocom_REI %in% liste_num_Paris, ccocom_REI := "056"]
  
  # Faire attention au typage
  carac_tf$ccocom_REI <- as.factor(carac_tf$ccocom_REI)
  carac_tf$ccodep <- as.factor(carac_tf$ccodep)
  SOUS_REI$DEPARTEMENT <- as.factor(SOUS_REI$DEPARTEMENT)
  SOUS_REI$COMMUNE <- as.factor(SOUS_REI$COMMUNE)  
  
  # Puis Merge merged et REI
  dt_merged_REI_loc <- merge(carac_tf, SOUS_REI, by.x = c("ccodep", "ccocom_REI"), by.y = c("DEPARTEMENT", "COMMUNE"), all.x = TRUE)
  
  # Merge avec le fichier Insee sur les types d'unité urbaine
  TUU_2020 <- data.table(readxl::read_excel(path = paste(repo_data, "/UU2020_au_01-01-2024.xlsx", sep = ""), sheet = "Composition_communale",skip=5))
  
  dt_merged_REI_loc[,CODGEO := paste0(ccodep,ccocom_REI)]
  
  dt_merged_REI_loc <- merge(dt_merged_REI_loc, TUU_2020, by.x = "CODGEO", by.y = "CODGEO", all.x = TRUE)
  
  return(dt_merged_REI_loc)
}


Renomer_colonnes_REI_2020 <- function(REI_loc){
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
  

  try(setnames(REI_loc, "Numéro_x0020_national_x0020_du_x0020_groupement", "Numéro.national.du.groupement"))
  try(setnames(REI_loc, "NUMERO_x0020_SIREN_x0020_DE_x0020_L_x0027_EPCI", "NUMERO.SIREN.DE.L"))
  try(setnames(REI_loc, "Libellé_x0020_du_x0020_Groupement", "Libellé.du.Groupement"))
  try(setnames(REI_loc, "option_x0020_fiscale_x0020_de_x0020_l_x0027_EPCI_x0020__x0028_FPA_x002C__x0020_FPU_x0020_ou_x0020_FPZ_x0029_", "option.fiscale.de.l'EPCI.(FPA,.FPU.ou.FPZ)"))
  try(setnames(REI_loc, "Forme_x0020_juridique_x0020_EPCI_x0020__x0028_CA_x002C__x0020_CU_x002C__x0020_CC_x002C__x0020_SAN_x0020_ou_x0020_Mét_x0029_", "Forme.juridique.EPCI.(CA,.CU,.CC,.SAN.ou.Mét)"))
  try(setnames(REI_loc, "Libellé_x0020_commune", "Libellé.commune"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_FRAIS_x0020_D_x0027_ASSIETTE_x002C__x0020_DEGREVEMENT_x002C__x0020__x0020_NON_x0020_VALEURS", "FB.-.FRAIS.D'ASSIETTE,.DEGREVEMENT,.NON.VALEURS"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_COMMUNE_x0020__x002F__x0020_BASE_x0020_NETTE", "FB.-.COMMUNE./.BASE.NETTE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_COMMUNE_x0020__x002F__x0020_TAUX_x0020_NET", "FB.-.COMMUNE./.TAUX.NET"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_COMMUNE_x0020__x002F__x0020_MONTANT_x0020_REEL", "FB.-.COMMUNE./.MONTANT.REEL"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_COMMUNE_x0020__x002F__x0020_NOMBRE_x0020_D_x0027_ARTICLES", "FB.-.COMMUNE./.NOMBRE.D'ARTICLES"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_COMMUNE_x0020__x002F__x0020_MONTANT_x0020_LISSAGE", "FB.-.COMMUNE./.MONTANT.LISSAGE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_GFP_x0020__x002F__x0020_BASE_x0020_NETTE", "FB.-.GFP./.BASE.NETTE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_GFP_x0020__x002F__x0020_TAUX_x0020_APPLICABLE_x0020_SUR_x0020_LE_x0020_TERRITOIRE_x0020_DE_x0020_LA_x0020_COMMUNE", "FB.-.GFP./.TAUX.APPLICABLE.SUR.LE.TERRITOIRE.DE.LA.COMMUNE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_GFP_x0020__x002F__x0020_TAUX_x0020_VOTE", "FB.-.GFP./.TAUX.VOTE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_GFP_x0020__x002F__x0020_MONTANT_x0020_REEL", "FB.-.GFP./.MONTANT.REEL"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_GFP_x0020__x002F__x0020_MONTANT_x0020_LISSAGE", "FB.-.GFP./.MONTANT.LISSAGE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_TSE_x0020__x002F__x0020_BASE_x0020_NETTE", "FB.-.TSE./.BASE.NETTE"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_TSE_x0020__x002F__x0020_TAUX_x0020_NET", "FB.-.TSE./.TAUX.NET"))
  try(setnames(REI_loc, "FB_x0020_-_x0020_DEPARTEMENT_x0020__x002F__x0020_TAUX_x0020_NET", "FB.-.DEP./.TAUX.NET"))
  
  return(REI_loc)
}


