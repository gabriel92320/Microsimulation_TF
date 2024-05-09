################################################################################
########### LES FONCTIONS POUR PREPARER LES GRAPHIQUES GGPLOT ##################
################################################################################

# Fonction qui calcule le montant total de TFPB (en Md euros) et le montant moyen
# (en euros) par décile de niveau de vie des ménages 2021:

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

# Fonction qui calcule le montant total de TFPB (en Md euros) et le montant moyen
# (en euros) par type des communes en termes d'unité urbaine 2020
# Type: commune dans une UU/ commune hors d'une UU.

Calcul_montant_tot_moy_TFPB_type_com_UU <- function(dt_merged_REI_loc, var_montant_TF = "Montant_TF_BRUT"){
  # Calcule le montant total de TF par type/statut de la commune
  # (en termes de UU), avec pondération
  
  # Type de commune (dans une UU / hors d'une UU) 
  # WARNING: pour pouvoir pondérer les montants de TFPB en fonction de la représentativité
  # de chaque ménage ds la pop, on se limite ici aux seules résidences principales (dont
  # on étudie la répartition par commune dans une UU ou hors d'une UU) 
    dt_merged_REI_loc_t <- dt_merged_REI_loc %>%
    tibble() %>%
    filter(!(is.na(TYPE_COMMUNE_UU)) & (Residence_principale==T)) %>%
    mutate(TYPE_COMMUNE_UU = factor(TYPE_COMMUNE_UU),
           ident21 = factor(ident21)) 
  
  # Merge avec la table ménage pour récup les poids:
    carac_men_tb <- as_tibble(carac_men) %>% mutate(ident = factor(ident))
    dt_merged_REI_loc_t <- dt_merged_REI_loc_t %>% 
      left_join(y = carac_men_tb, 
                by = c("ident21" = "ident"))
    
  # Calculs avec pondération:
  TFPB_type_com_UU_p <- dt_merged_REI_loc_t %>%
    mutate(poi2=2*poi) %>%
    group_by(TYPE_COMMUNE_UU) %>%
    summarise(Nb_menages = sum(poi2,na.rm = T),
              TFPB_tot = sum(get(var_montant_TF)*poi2,na.rm = T)
    ) %>%
    mutate(TFPB_moy = TFPB_tot/Nb_menages) %>%
    mutate(Nb_menages2 = Nb_menages/1e6, #en millions de ménages
           TFPB_tot2 = TFPB_tot/1e9 #en milliard d'euros
    ) %>%
    select(TYPE_COMMUNE_UU,Nb_menages2,TFPB_tot2,TFPB_moy) %>%
    rename(Nb_menages=Nb_menages2,TFPB_tot=TFPB_tot2
    )
  
  
  return(TFPB_type_com_UU_p)
}

# Fonction qui calcule le montant total de TFPB (en Md euros) et le montant moyen
# (en euros) par statut des communes en termes d'unité urbaine 2020
# Statut: H (hors UU) / C (Ville-centre) / B (Banlieue) / I (Ville isolée)

Calcul_montant_tot_moy_TFPB_statut_com_UU <- function(dt_merged_REI_loc, var_montant_TF = "Montant_TF_BRUT"){
  # Calcule le montant total de TF par type/statut de la commune
  # (en termes de UU), avec pondération
  
  # Type de commune (dans une UU / hors d'une UU) 
  # WARNING: pour pouvoir pondérer les montants de TFPB en fonction de la représentativité
  # de chaque ménage ds la pop, on se limite ici aux seules résidences principales (dont
  # on étudie la répartition par commune dans une UU ou hors d'une UU) 
  dt_merged_REI_loc_t <- dt_merged_REI_loc %>%
    tibble() %>%
    filter(!(is.na(STATUT_COM_UU)) & (Residence_principale==T)) %>%
    mutate(STATUT_COM_UU = factor(STATUT_COM_UU),
           ident21 = factor(ident21)) %>%
    mutate(STATUT_COM_UU=fct_recode(STATUT_COM_UU,"Hors UU"="H","Ville-centre"="C",
                                    "Banlieue"="B","Ville isolée"="I"))
  
  # Merge avec la table ménage pour récup les poids:
  carac_men_tb <- as_tibble(carac_men) %>% mutate(ident = factor(ident))
  dt_merged_REI_loc_t <- dt_merged_REI_loc_t %>% 
    left_join(y = carac_men_tb, 
              by = c("ident21" = "ident"))
  
  # Calculs avec pondération:
  TFPB_statut_com_UU_p <- dt_merged_REI_loc_t %>%
    mutate(poi2=2*poi) %>%
    group_by(STATUT_COM_UU) %>%
    summarise(Nb_menages = sum(poi2,na.rm = T),
              TFPB_tot = sum(get(var_montant_TF)*poi2,na.rm = T)
    ) %>%
    mutate(TFPB_moy = TFPB_tot/Nb_menages) %>%
    mutate(Nb_menages2 = Nb_menages/1e6, #en millions de ménages
           TFPB_tot2 = TFPB_tot/1e9 #en milliard d'euros
    ) %>%
    select(STATUT_COM_UU,Nb_menages2,TFPB_tot2,TFPB_moy) %>%
    rename(Nb_menages=Nb_menages2,TFPB_tot=TFPB_tot2
    )
  
  
  return(TFPB_statut_com_UU_p)
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
