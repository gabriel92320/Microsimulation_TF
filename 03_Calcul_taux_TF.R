################################################################################
########### LES FONCTIONS POUR CALCULER LES TAUX DE TF BRUTES ET NETTES ########
################################################################################


# Trouver la résidence principale
Assigner_res_principale <- function(dt_loc) {
  # Trouver l'indice de la ligne ayant la valeur bipeva maximale pour chaque ménage
  indices_max <- dt_loc[, .I[which.max(bipeva)], by = ident21]$V1
  
  dt_loc[, Residence_principale := FALSE]
  # Attribuer TRUE aux lignes avec les indices trouvés
  dt_loc[indices_max, Residence_principale := TRUE]
}





Calculer_taux_brut <- function(dt_merged_REI_loc){
  # Calcul la TF brut par logement
  # Fixage des types
  dt_merged_REI_loc$vlbaia <- as.numeric(dt_merged_REI_loc$vlbaia)
  dt_merged_REI_loc$bipeva <- as.numeric(dt_merged_REI_loc$bipeva)
  dt_merged_REI_loc$bateom <- as.numeric(dt_merged_REI_loc$bateom)
  dt_merged_REI_loc$mvltieomx <- as.numeric(dt_merged_REI_loc$mvltieomx)
  dt_merged_REI_loc$baomec <- as.numeric(dt_merged_REI_loc$baomec)
  
  
  dt_merged_REI_loc[, Montant_communal_TF := FB_COMMUNE_TAUX_NET * bipeva/100]
  dt_merged_REI_loc[, Montant_GFP_TF := FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE * bipeva/100]
  dt_merged_REI_loc[, Montant_TF_BRUT := Montant_communal_TF + Montant_GFP_TF]
  
  dt_merged_REI_loc[, Montant_TF_BRUT_proratise := Montant_TF_BRUT/nb_prop]
  
  return(dt_merged_REI_loc)
}


Calculer_taux_net <- function(dt_merged_REI_loc, carac_men_loc){
  # Exonérations et dégrèvements ==> On se place à l'échelle du logement pour pouvoir prendre en compte l'aspet résidence principale/secondaire
  # Choix méthodo : pour les logements multipropriétaires, on considère que la résidence principale est le logement avec la plus haute valeur locative ==> On suppose que c'est aussi le logement qui a la plus haute valeur (+ intérêt fiscal du ménage optimisateur)
  
  liste_ident_exonerees_res_princ <- carac_men_loc[(aspa == '1') |
                                                     # (age_pr >= 75 & rfr < 11885) |
                                                     (aah == "1" & rfr < 11885), ]$ident
  liste_ident_exonerees_toutes_res <- carac_men_loc[(age_pr >= 75 & rfr < 11885), ]$ident
  liste_ident_degreves_res_princ <- carac_men_loc[age_pr >= 65 & rfr < 11885 & age_pr < 75, ]$ident
  
  
  Assigner_res_principale(dt_merged_REI_loc)
  
  
  # Assigner dégrèvement ou exonération aux logements
  dt_merged_REI_loc[ident21 %in% liste_ident_exonerees_res_princ & Residence_principale == TRUE, Logement_exonere := TRUE]
  dt_merged_REI_loc[ident21 %in% liste_ident_exonerees_toutes_res, Logement_exonere := TRUE]
  dt_merged_REI_loc[ident21 %in% liste_ident_degreves_res_princ & Residence_principale == TRUE, Logement_degreve := TRUE]
  
  dt_merged_REI_loc[is.na(Logement_exonere), Logement_exonere:= FALSE]
  dt_merged_REI_loc[is.na(Logement_degreve), Logement_degreve:= FALSE]
  
  
  # Calculer bipeva_net qui prend en compte exonération et dégrèvement
  dt_merged_REI_loc[, Montant_TF_NETTE := Montant_TF_BRUT] # On calcule le net
  dt_merged_REI_loc[Logement_degreve == TRUE, Montant_TF_NETTE := Montant_TF_NETTE - 100] # On retire les logements dégrévés ==> QUESTION : SI ON TOMBE SUR UN NB NEGATIF ????? 8 lignes concernées
  dt_merged_REI_loc[Logement_exonere == TRUE, Montant_TF_NETTE := 0] # On retire les logements exonérés

  dt_merged_REI_loc[, Montant_TF_NETTE_proratise := Montant_TF_NETTE/nb_prop]
  
  return(dt_merged_REI_loc)
}
