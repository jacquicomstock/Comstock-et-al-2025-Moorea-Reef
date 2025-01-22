##Run linear models on individual ASVs and families

# make linears model code a function
model1 <- function(x) {lm(x ~ reef$Reef.Type*reef$Reef.Side)}
model2 <- function(x) {lmer(x ~ reef$Reef.Type + (1|reef$Reef.Side))}
model3 <- function(x) {lmer(x ~ biogeo.trim*reef$Reef.Type + (1|reef$Reef.Side))}

# run linear models on culled ASVs
lm1 <- apply(norm_reef_asvcull,2,FUN=model1)
lmer1 <- apply(norm_reef_asvcull,2,FUN=model2) #follow up with post hoc test

# run linear models on culled families
lm2 <- apply(norm_reef_famcull,2,FUN=model1)
lmer2 <- apply(norm_reef_famcull,2,FUN=model2)

# run linear models on biogeocehmical parameters
lmer3 <- apply(biogeo.trim,2,FUN=model2)
lmer4 <- apply(biogeo.trim,2,FUN=model3)


##Anova and filtration for "lm" simple linear models

#run anova
lm1.anova <- as.data.frame(lapply(lm1, FUN=anova))
lm2.anova <- as.data.frame(lapply(lm2, FUN=anova))

#remove "residuals" row, then subset every 5th column
lm1.anova.trim <- lm1.anova[1:3,]
t_lm1.anova <- t(as.data.frame(lm1.anova.trim))
lm1.pvals <- as.data.frame(t_lm1.anova[seq(from=5,to=nrow(t_lm1.anova),by=5),])
colnames(lm1.pvals) <- c("reef_habitat","reef_side","habitat*side")

lm2.anova.trim <- lm2.anova[1:3,]
t_lm2.anova <- t(as.data.frame(lm2.anova.trim))
lm2.pvals <- as.data.frame(t_lm2.anova[seq(from=5,to=nrow(t_lm2.anova),by=5),])
colnames(lm2.pvals) <- c("reef_habitat","reef_side","habitat*side")

#change column names to taxonomy names
#p adjust
lm1.habitat.pvals <- as.data.frame(lm1.pvals$reef_habitat)
rownames(lm1.habitat.pvals) <- colnames(norm_reef_asvcull)
colnames(lm1.habitat.pvals) <- "reef_habitat"
lm1.habitat.pvals$padjust.habitat <- p.adjust(lm1.habitat.pvals$reef_habitat,method="BH")

lm1.side.pvals <- as.data.frame(lm1.pvals$reef_side)
rownames(lm1.side.pvals) <- colnames(norm_reef_asvcull)
colnames(lm1.side.pvals) <- "reef_side"
lm1.side.pvals$padjust.side <- p.adjust(lm1.side.pvals$reef_side,method="BH")

lm1.habitatside.pvals <- as.data.frame(lm1.pvals$'habitat*side')
rownames(lm1.habitatside.pvals) <- colnames(norm_reef_asvcull)
colnames(lm1.habitatside.pvals) <- "habitat*side"
lm1.habitatside.pvals$padjust.habitat_side <- p.adjust(lm1.habitatside.pvals$`habitat*side`,method="BH")


lm2.habitat.pvals <- as.data.frame(lm2.pvals$reef_habitat)
rownames(lm2.habitat.pvals) <- colnames(norm_reef_famcull)
colnames(lm2.habitat.pvals) <- "reef_habitat"
lm2.habitat.pvals$padjust.habitat <- p.adjust(lm2.pvals$reef_habitat,method="BH")

lm2.side.pvals <- as.data.frame(lm2.pvals$reef_side)
rownames(lm2.side.pvals) <- colnames(norm_reef_famcull)
colnames(lm2.side.pvals) <- "reef_side"
lm2.side.pvals$padjust.side <- p.adjust(lm2.pvals$reef_side,method="BH")

lm2.habitatside.pvals <- as.data.frame(lm2.pvals$'habitat*side')
rownames(lm2.habitatside.pvals) <- colnames(norm_reef_famcull)
colnames(lm2.habitatside.pvals) <- "habitat*side"
lm2.habitatside.pvals$padjust.habitat_side <- p.adjust(lm2.pvals$`habitat*side`,method="BH")

#filter out all significant ASVs
lm1.habitat.sub <- lm1.habitat.pvals[lm1.habitat.pvals$padjust.habitat<=.05,]
lm1.side.sub <- lm1.side.pvals[lm1.side.pvals$padjust.side<=.05,]
lm1.habitatside.sub <- lm1.habitatside.pvals[lm1.habitatside.pvals$'padjust.habitat_side'<=.05,]

lm2.habitat.sub <- lm2.habitat.pvals[lm2.habitat.pvals$padjust.habitat<=.05,]
lm2.side.sub <- lm2.side.pvals[lm2.side.pvals$padjust.side<=.05,]
lm2.habitatside.sub <- lm2.habitatside.pvals[lm2.habitatside.pvals$'padjust.habitat_side'<=.05,]

#write csv file
write.csv(lm1.habitat.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lm1.asv_habitat.csv")
write.csv(lm1.side.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lm1.asv_side.csv")
write.csv(lm1.habitatside.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lm1.asv_habitatside.csv")

write.csv(lm2.habitat.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lm2.fam_habitat.csv")
write.csv(lm2.side.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lm2.fam_side.csv")
write.csv(lm2.habitatside.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lm2.fam_habitatside.csv")


##Anova and filtration for "lmer" linear mixed models

#test each mixed model for a singularity
#if all of the models are singular then you have a problem estimating the random effects
lmer1.singularity=lapply(lmer1,FUN=isSingular)
lmer2.singularity=lapply(lmer2,FUN=isSingular)

#test the significance of each variable
#first, make a function that performs anova (Kenward-Roger). I was told do use this specific anova method by a statistician on my committee, so that's why I used it.
anova.kw=function(x) { 
  results=anova(x,ddf="Kenward-Roger")
  return(results)
}

#run anova on all mixed models
lmer1.results=lapply(lmer1,FUN=anova.kw)
lmer2.results=lapply(lmer2,FUN=anova.kw)
lmer3.results=lapply(lmer3,FUN=anova.kw)
lmer4.results=lapply(lmer4,FUN=anova.kw)

#extract p values
t_lmer1.results <- t(as.data.frame(lmer1.results))
lmer1.pvals <- as.data.frame(t_lmer1.results[seq(from=6,to=nrow(t_lmer1.results),by=6),])

t_lmer2.results <- t(as.data.frame(lmer2.results))
lmer2.pvals <- as.data.frame(t_lmer2.results[seq(from=6,to=nrow(t_lmer2.results),by=6),])

t_lmer3.results <- t(as.data.frame(lmer3.results))
lmer3.pvals <- as.data.frame(t_lmer3.results[seq(from=6,to=nrow(t_lmer3.results),by=6),])

t_lmer4.results <- t(as.data.frame(lmer4.results))
lmer4.pvals <- as.data.frame(t_lmer4.results[seq(from=6,to=nrow(t_lmer4.results),by=6),])

#Work up p values.
colnames(lmer1.pvals) <- "reef_type"
lmer1.pvals$padjust <- p.adjust(lmer1.pvals$reef_type,method="BH") 

colnames(lmer2.pvals) <- "reef_type"
lmer2.pvals$padjust <- p.adjust(lmer2.pvals$reef_type,method="BH")

colnames(lmer3.pvals) <- "pvals"
lmer3.pvals$padjust <- p.adjust(lmer3.pvals$pvals,method="BH")

colnames(lmer4.pvals) <- c("biogeo","habitat","biogeo_habitat")
lmer4.pvals$padjust.habitat <- p.adjust(lmer4.pvals$habitat,method="BH")
lmer4.pvals$padjust.biogeohabitat <- p.adjust(lmer4.pvals$biogeo_habitat,method="BH")

#Change rownames to correspond with tested ASV names.
rownames(lmer1.pvals)=colnames(norm_reef_asvcull)
rownames(lmer2.pvals)=colnames(norm_reef_famcull)
rownames(lmer3.pvals)=colnames(biogeo.trim)
rownames(lmer4.pvals)=colnames(biogeo.trim)

#subset lmer3 and 4
lmer4.pvals.hab <- lmer4.pvals[,c(2,4)]
lmer4.pvals.biohab <- lmer4.pvals[,c(3,5)]
  
#subset the mixed.mod.pvals df for significant (p<=.05)
lmer1.sub <-lmer1.pvals[lmer1.pvals$padjust<=.05,]
lmer2.sub <-lmer2.pvals[lmer2.pvals$padjust<=.05,]
lmer3.sub <-lmer3.pvals[lmer3.pvals$padjust<=.05,]
lmer4.sub.hab <-lmer4.pvals.hab[lmer4.pvals.hab$padjust.habitat<=.05,]
lmer4.sub.biohab <-lmer4.pvals.biohab[lmer4.pvals.biohab$padjust.biogeohabitat<=.05,]

#write csv file
write.csv(lmer1.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lmer1.csv")
write.csv(lmer2.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lmer2.csv")
write.csv(lmer3.sub,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lmer3.csv")
write.csv(lmer4.sub.hab,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lmer4.habitat.csv")
write.csv(lmer4.sub.hab,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/lmer4.biogeohabitat.csv")
