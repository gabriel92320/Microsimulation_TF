################################################################################
########### LES FONCTIONS POUR PREPARER LES DT EXPLOITABLES ####################
################################################################################


Faire_dt_SOUS_REI <- function(liste_cols_REI_loc){
  # Le fichier REI est trop volumineux, pour travailler dessus on va ne sélectionner que quelques colonnes
  # Cette fonction ne renvoie rien, mais sauvegarde REI_2021_SELECT
  
  REI_21 <- data.table(read.xlsx(paste(repo_data, "REI_2021.xlsx", sep = "/")))
  SOUS_REI <- REI_21[, ..liste_cols_REI]
  rm(REI_21)
  save(SOUS_REI, file = paste(repo_bases_intermediaires, "REI_2021_SELECT.RData", sep = "/"))
}

Importer_et_merge_DT_REI <- function(liste_cols_REI_loc){
  # Importe et merge : carac_men, carac_tf et SOUS_REI. On ne garde que quelques colonnes de SOUS_REI
  # Retourne le dt merged
  # ATTENTION SI ON AJOUTE DES COLONNES IL VA FALLOIR SANS DOUTE AJOUTER DES SETNAMES
  
  # Importer le REI et sélectionner les colonnes
  load(paste(repo_bases_intermediaires, "REI_2021_SELECT.RData", sep = "/"))
  SOUS_REI <- SOUS_REI[,..liste_cols_REI_loc]
  
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
  dt_merged_REI_loc <- merge(dt_merged, SOUS_REI, by.x = c("ccodep", "ccocom_REI"), by.y = c("DEPARTEMENT", "COMMUNE"), all.x = TRUE)
  
  return(dt_merged_REI_loc)
}