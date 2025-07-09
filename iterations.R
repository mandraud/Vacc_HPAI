### Canards Chair

tmp=NULL
for (run in 1:100){
  tmp[[run]]=Vaccination_Modele_Chair(nweeks=26,ninit = MEP[1,2],Sale=Sale,MEP=MEP)
}

iterations=NULL
for (i in 1:100){
  iterations=rbind(iterations,cbind(iter_id=i,tmp[[i]][[1]]) )
}
counts <- iterations %>%
  group_by(week, iter_id, statut) %>%
  summarise(count = n(), .groups = "drop")

# Graphique ggplot
plot_Chair <- ggplot(counts, aes(x = factor(week), y = count)) +
  geom_boxplot(aes(color = statut, fill = statut), alpha = 0.4,outlier.shape = NA ) +  # Remplissage et couleur basés sur 'statut'
  scale_color_manual(values = c("orange3", "seashell4", 
                                "seagreen2", "seagreen4", "chartreuse3")) +  # Couleurs des contours
  scale_fill_manual(values = c("orange3", "seashell4", 
                               "seagreen2", "seagreen4", "chartreuse3")) +   # Couleurs du remplissage
  facet_wrap(~statut, shrink = TRUE) +
  theme(legend.position = 'none') +  # Supprimer la légende
  labs(
    x = "Weeks",
    y = "Number of flocks",
    title = "Meat ducks"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotation des étiquettes de l'axe x
  )


tmp=NULL
for (run in 1:100){
  tmp[[run]]=Vaccination_Modele_Gras(nweeks=26,nNonV3 = MEP_Gras[1,2],
                          nV3=MEP_Gras[1,3],Sale=Sale_Gras,MEP=MEP_Gras)
}


### Canards gras
iterations=NULL
for (i in 1:100){
  iterations=rbind(iterations,cbind(iter_id=i,tmp[[i]][[1]]) )
}
counts <- iterations %>%
group_by(week, iter_id, statut) %>%
summarise(count = n(), .groups = "drop")
counts$color=c("orange3", "seashell4", 
               "seagreen2", "seagreen4", "chartreuse3")[as.numeric(factor(counts$statut))]
# Graphique ggplot
plot_Gras <- ggplot(counts, aes(x = factor(week), y = count)) +
  geom_boxplot(aes(color = statut, fill = statut), alpha = 0.4,outlier.shape = NA ) +  # Remplissage et couleur basés sur 'statut'
  scale_color_manual(values = c("orange3", "seashell4",
                                "seagreen2", "seagreen4", "chartreuse3")) +  # Couleurs des contours
  scale_fill_manual(values = c("orange3", "seashell4", 
                               "seagreen2", "seagreen4", "chartreuse3")) +   # Couleurs du remplissage
  facet_wrap(~statut, shrink = TRUE) +
  theme(legend.position = 'none') +  # Supprimer la légende
  labs(
    x = "Weeks",
    y = "Number of flocks",
    title = "Fattening ducks"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotation des étiquettes de l'axe x
  )


