#Run NMDS ordination and create overlaying environmental vectors of the biogeochemical parameters found to significantly change between reef habitats
#Those variables are: chl a, POC, BACT, DOC, Phosphate, Nitrate+Nitrite
nmds = metaMDS(norm_reef, distance = "bray", trymax = 1000)
data.scores = as.data.frame(scores(nmds))
env = reef[,c(34,35,40,47,55,61)]

en = envfit(nmds, env, permutations = 999, na.rm = TRUE)

en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)

 ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2, color=reef$Reef.Side.Habitat)) + 
  geom_point(data = data.scores, size = 2, alpha = 0.5) + 
  #scale_colour_manual(values = c( "springgreen4", "orange3")) + 
   #xlim(-1.1,1.3) +
   #ylim(-.6,0.6) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =1, alpha = 0.5, colour = "black") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "black", 
            fontface = "bold", label = row.names(en_coord_cont), size=2) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "black"), 
         legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "black"), 
        legend.text = element_text(size = 9, colour = "black")) 
