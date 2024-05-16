################################################################################
########### LES FONCTIONS QUI PRODUISENT DES GRAPHIQUES GGPLOT #################
################################################################################

Faire_graphique_barplot <- function(data_loc, x, y,xlabel, ylabel, ysuffix = "M€", titre_save, titre_graphe, sous_titre_graphe){
  # Fait et sauvegarde un graphique barplot
  
  graph1 <- ggplot(data_loc) +
    aes(x = .data[[x]], y = .data[[y]]) +
    geom_col(fill = "#112446") +
    labs(
      x = xlabel,
      y = ylabel,
      title = titre_graphe,
      subtitle = sous_titre_graphe
    ) +
    scale_y_continuous(labels = scales::dollar_format(
      prefix = "",
      suffix = ysuffix,
      big.mark = " ",
      decimal.mark = ",")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
          text = element_text(size = 18),  # Changer la taille de la police générale
          axis.title = element_text(size = 18),  # Changer la taille de la police des titres d'axe
          axis.text = element_text(size = 18),  # Changer la taille de la police des étiquettes d'axe
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Changer la taille de la police du titre du graphique
          legend.text = element_text(size = 18),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position="bottom") 
  
  print(graph1)
  ggsave(titre_save, graph1 ,  width = 297, height = 210, units = "mm")
  
}


Faire_graphique_barplot_avec_fill <- function(data_loc, x, y, fill, xlabel, ylabel, filllabel, ysuffix = "M€", titre_save, titre_graphe, sous_titre_graphe){
  # Fait et sauvegarde un graphique barplot
  
  graph1 <- ggplot(data_loc) +
    aes(x = .data[[x]], y = .data[[y]], fill = str_wrap(.data[[fill]], 18)) +
    geom_col(position = "dodge2") +
    labs(
      x = xlabel,
      y = ylabel,
      fill = filllabel,
      title = titre_graphe,
      subtitle = sous_titre_graphe
    ) +
    scale_y_continuous(labels = scales::dollar_format(
      prefix = "",
      suffix = ysuffix,
      big.mark = " ",
      decimal.mark = ",")) +
    theme_minimal() +
    scale_fill_viridis(discrete = TRUE) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
          text = element_text(size = 18),  # Changer la taille de la police générale
          axis.title = element_text(size = 18),  # Changer la taille de la police des titres d'axe
          axis.text = element_text(size = 18),  # Changer la taille de la police des étiquettes d'axe
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Changer la taille de la police du titre du graphique
          legend.text = element_text(size = 18),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position="bottom") 
  
  print(graph1)
  ggsave(titre_save, graph1 ,  width = 297, height = 210, units = "mm")
  
}




Faire_carte_departements <- function(data_loc, titre_graphe, sous_titre_graphe, titre_save){
  # Plot de la carte
  graph1 <- ggplot() +
    geom_sf(data = data_loc, aes(fill = Fill_carte)) +
    scale_fill_viridis() +
    theme_void() +
    labs(
      title = titre_graphe,
      subtitle = sous_titre_graphe,
      fill = filllabel) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
          text = element_text(size = 18),  # Changer la taille de la police générale
          axis.title = element_text(size = 18),  # Changer la taille de la police des titres d'axe
          axis.text = element_text(size = 18),  # Changer la taille de la police des étiquettes d'axe
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Changer la taille de la police du titre du graphique
          legend.text = element_text(size = 18),
          plot.subtitle = element_text(hjust = 0.5)) 
  
  
  print(graph1)
  ggsave(titre_save, graph1 ,  width = 297, height = 210, units = "mm")
}

