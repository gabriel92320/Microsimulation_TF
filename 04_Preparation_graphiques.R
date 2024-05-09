################################################################################
########### LES FONCTIONS POUR PREPARER LES GRAPHIQUES GGPLOT ##################
################################################################################


Calcul_montant_tot_moy_TFPB_nivviem <- function(dt_merged_REI_loc, var_montant_TF = "Montant_TF_BRUT"){
  # Calcule le montant total de TF par niveau de vie, avec pondération
  
  # Taux moyen de TFPB brute par décile de niveau de vie en 2021:
  TFPB_menages <- dt_merged_REI_loc[, .(TFPB = sum(get(var_montant_TF),na.rm = T)), by = 'ident21']
  
  # Merge avec ménage pour récup les caractéristiques
  TFPB_menages$ident21 <- as.factor(TFPB_menages$ident21)
  carac_men$ident <- as.factor(carac_men$ident)
  TFPB_menages_tb <- merge(carac_men, TFPB_menages, by.x = "ident", by.y = "ident21")
  
  TFPB_menages_tb <- TFPB_menages_tb %>%
    filter(!(is.na(decile_ndv))) %>%
    mutate(decile_ndv = factor(decile_ndv)) %>%
    mutate(decile_ndv=fct_recode(decile_ndv,"D1"="1","D2"="2","D3"="3","D4"="4",
                                 "D5"="5","D6"="6","D7"="7","D8"="8","D9"="9",
                                 "D10"="10"))
  
  # Calculs avec pondération:
  TFPB_decile_ndv_p <- TFPB_menages_tb %>%
    mutate(poi2=2*poi) %>%
    group_by(decile_ndv) %>%
    summarise(Nb_menages = sum(poi2,na.rm = T),
              TFPB_tot = sum(TFPB*poi2,na.rm = T)
    ) %>%
    mutate(TFPB_moy = TFPB_tot/Nb_menages) %>%
    mutate(Nb_menages2 = Nb_menages/1e6, #en millions de ménages
           TFPB_tot2 = TFPB_tot/1e9 #en milliard d'euros
    ) %>%
    select(decile_ndv,Nb_menages2,TFPB_tot2,TFPB_moy) %>%
    rename(Nb_menages=Nb_menages2,TFPB_tot=TFPB_tot2
    )
  
  
  return(TFPB_decile_ndv_p)
}





Calcul_assiette_tx_apparent_TFPB <- function(dt_merged_REI_loc, var_montant_TF = "Montant_TF_BRUT"){
  # Calcul du taux moyen apparent de TFPB par décile de niveau de vie en 2021:
  # Récupération de l'assiette de la TFPB par ménage
  assiette_TFPB_tb <- carac_tf %>%
    as_tibble() %>%
    select(ident21,bipeva) %>%
    mutate(bipeva=as.numeric(bipeva)) %>%
    group_by(ident21) %>%
    summarise(assiette_TFPB=sum(bipeva,na.rm = T)) 
  
  # On joint avec carac_men pour récupérer caractéristiques du ménage
  TFPB_menages <- dt_merged_REI_loc[, .(TFPB = sum(get(var_montant_TF),na.rm = T)), by = 'ident21']
  TFPB_menages$ident21 <- as.factor(TFPB_menages$ident21)
  carac_men$ident <- as.factor(carac_men$ident)
  TFPB_menages_tb <- merge(carac_men, TFPB_menages, by.x = "ident", by.y = "ident21")
  
  # Jointure avec les caractéristiques des ménages + TFPB brute simulée 2021
  assiette_TFPB_tb$ident21 <- as.factor(assiette_TFPB_tb$ident21)
  TFPB_menages_tb <- merge(TFPB_menages_tb,assiette_TFPB_tb, by.x = "ident", by.y = "ident21", all.x = TRUE)
  
  # Calcul du taux moyen apparent de TFPB par ménage
  TFPB_menages_tb <- TFPB_menages_tb %>%
    mutate(tx_apparent_TFPB=TFPB/assiette_TFPB*100,
           poi2 = poi*2)
  
  return(TFPB_menages_tb)
}
