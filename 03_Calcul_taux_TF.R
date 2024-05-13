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


Calculer_taux_net <- function(dt_merged_REI_loc, carac_men_loc, annee_loc = 2021, utiliser_dvldif2a = TRUE){
  # Exonérations et dégrèvements ==> On se place à l'échelle du logement pour pouvoir prendre en compte l'aspet résidence principale/secondaire
  # Choix méthodo : pour les logements multipropriétaires, on considère que la résidence principale est le logement avec la plus haute valeur locative ==> On suppose que c'est aussi le logement qui a la plus haute valeur (+ intérêt fiscal du ménage optimisateur)
  
  
  #### Les propriétaires exonérés ou dégrévés sur critères sociaux
  liste_ident_exonerees_res_princ <- carac_men_loc[(aspa == '1') |
                                                     (age_pr >= 75 & rfr < 11885) |
                                                     (aah == "1" & rfr < 11885), ]$ident
  # liste_ident_exonerees_toutes_res <- carac_men_loc[(age_pr >= 75 & rfr < 11885), ]$ident
  liste_ident_degreves_res_princ <- carac_men_loc[age_pr >= 65 & rfr < 11885 & age_pr < 75, ]$ident
  
  
  Assigner_res_principale(dt_merged_REI_loc)
  
  
  # Assigner dégrèvement ou exonération aux logements
  dt_merged_REI_loc[ident21 %in% liste_ident_exonerees_res_princ & Residence_principale == TRUE, Logement_exonere := TRUE]
  dt_merged_REI_loc[ident21 %in% liste_ident_exonerees_res_princ & Residence_principale == TRUE, Raison_exoneration := "Crit_sociaux"]
  # dt_merged_REI_loc[ident21 %in% liste_ident_exonerees_toutes_res, Logement_exonere := TRUE]
  dt_merged_REI_loc[ident21 %in% liste_ident_degreves_res_princ & Residence_principale == TRUE, Logement_degreve := TRUE]
  dt_merged_REI_loc[ident21 %in% liste_ident_degreves_res_princ & Residence_principale == TRUE, Raison_exoneration := "Crit_sociaux"]
  
  
  dt_merged_REI_loc[is.na(Logement_exonere), Logement_exonere:= FALSE]
  dt_merged_REI_loc[is.na(Logement_degreve), Logement_degreve:= FALSE]
  
  #### Les logements exonérés du fait de construction ou reconstruction récente
  dt_merged_REI_loc[annee_loc >= jandeb & annee_loc < janimp, Logement_exonere := TRUE]
  dt_merged_REI_loc[ident21 %in% liste_ident_degreves_res_princ & Residence_principale == TRUE, Raison_exoneration := "Constru_recente"]
  # JANDEB = année de début d’exonération
  # JANIMP = année de retour à imposition
  
  #### Les exonérations "autres" à partir de la variable ==> Dans les faits il n'y en a aucun
  dt_merged_REI_loc[Logement_exonere == FALSE & Logement_degreve == FALSE & pexb_C > 0, Logement_exonere := FALSE]
  dt_merged_REI_loc[Logement_exonere == FALSE & Logement_degreve == FALSE & pexb_C > 0, Raison_exoneration := "Autre"] 
  
  
  if(!utiliser_dvldif2a){
    # Calculer Montant_TF_NETTE qui prend en compte exonération et dégrèvement
    dt_merged_REI_loc[, Montant_TF_NETTE := Montant_TF_BRUT] # On calcule le net
    dt_merged_REI_loc[Logement_degreve == TRUE, Montant_TF_NETTE := Montant_TF_NETTE - 100] # On retire les logements dégrévés ==> QUESTION : SI ON TOMBE SUR UN NB NEGATIF ????? 8 lignes concernées
    dt_merged_REI_loc[Logement_exonere == TRUE, Montant_TF_NETTE := 0] # On retire les logements exonérés
    dt_merged_REI_loc[, Montant_TF_NETTE_proratise := Montant_TF_NETTE/nb_prop]
  }else{
    # En version 2, avec la variable dvldif2a
    dt_merged_REI_loc[, Montant_TF_NETTE := Montant_TF_BRUT]
    dt_merged_REI_loc[!is.na(dvldif2a), Montant_TF_NETTE := (FN_GFP_TAUX_APPLICABLE_SUR_LE_TERRITOIRE_DE_LA_COMMUNE + FB_COMMUNE_TAUX_NET)*dvldif2a/(2*100)] # On recalcule la TF nette
    dt_merged_REI_loc[, Montant_TF_NETTE_proratise := Montant_TF_NETTE/nb_prop]
  
  }
  
  ### QQ explorations pour se rassurer
  # nrow(dt_merged_REI_loc[pexb_C != pexb_TS]) # Tous les logements sont comptés comme "Exonérés" par nous, et il y en a très peu ==> Très peu de diff entre les 3 variables ouf
  # nrow(dt_merged_REI_loc[pexb_C != pexb_GC])
  # table(dt_merged_REI_loc[pexb_C != pexb_TS]$Logement_exonere)

  # nrow(dt_merged_REI_loc[pexb_C > 0 & !Logement_exonere])
  # table(dt_merged_REI_loc[Logement_exonere == T,]$pexb_C)
  # nrow(dt_merged_REI_loc[Logement_exonere == T & pexb_C < 10000,]) # Environ 1% des lignes sont comptés comme "Exonérées" et ont un tx d'exonération inférieur à 100%
  # table(dt_merged_REI_loc[Logement_exonere == F,]$pexb_C) # Tous les logements qu'on compte comme "non exonérés" ont un tx d'éxonération NAN => 0% ==> On n'en a oublié aucun
  
  # dt_merged_REI_loc[Logement_exonere == F & !is.na(dvldif2a)] # Tous les logements où dvldif2a est renseigné sont des logements exonérés
  # table(dt_merged_REI_loc[!is.na(dvldif2a)]$dvldif2a == dt_merged_REI_loc[!is.na(dvldif2a)]$dvlpera * dt_merged_REI_loc[!is.na(dvldif2a)]$pexb_C/10000) 
  
  
  #3139  3545   311  1187  2290  3545  1054  1054  3762  1596  1596  2241   266
  return(dt_merged_REI_loc)
}
