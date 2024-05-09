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
          text = element_text(size = 20),  # Changer la taille de la police générale
          axis.title = element_text(size = 20),  # Changer la taille de la police des titres d'axe
          axis.text = element_text(size = 20),  # Changer la taille de la police des étiquettes d'axe
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Changer la taille de la police du titre du graphique
          legend.text = element_text(size = 20),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position="bottom") 
  
  print(graph1)
  ggsave(titre_save, graph1 ,  width = 297, height = 210, units = "mm")
  
}

