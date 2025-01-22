#create family table
taxa<- read.csv("C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Oceanic/dada2_output_untrimmed/Working files/rar3000/KM_taxa_NoChl_rar3000.csv", header=TRUE,sep=",",row.names=1)

#concat phylum, class, order, and family so family NAs don't lump together
tr_reef <- t(m_reef/100)
phylum_class_order_family <- as.matrix(paste(taxa$Phylum,taxa$Class,taxa$Order,taxa$Family, sep = "_", collapse = NULL))
#rownames(phylum_class_order_family) <- rownames(tr_reef)
#bind taxa and ASV columns
#reef_taxa_asv <- bind_cols(phylum_class_order_family,tr_reef)


#Iport data
phyl_class_order_fam_numbered <- as.data.frame(read.csv("C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/phyl_class_order_fam_numbered.csv", header=TRUE,sep=","))
colnames(phyl_class_order_fam_numbered) <- c("asv_num","phyl_class_order_family")

taxa_numbered <- as.data.frame(read.csv("C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/taxa_rar3000_reeftrim_numbered.csv", header=TRUE,sep=","))
colnames(taxa_numbered) <- c("asv_num", "last_numbers", "asv_name")

#filter out only the remaining reef ASVs from the complete ASV table
taxa_phyl_trim <- inner_join(phyl_class_order_fam_numbered,taxa_numbered, by="asv_num")

#create trimmed family df
familytaxa_trim <- as.matrix(taxa_phyl_trim$phyl_class_order_family)

#add family names to asv df
tr_reef_fam <- as.matrix(t(m_reef))
row.names(tr_reef_fam) <- familytaxa_trim

#sum ASVs in same family to create family df
family.merge <- rowsum(tr_reef_fam, row.names(tr_reef_fam))

#create trimmed ASV df
asvtaxa_trim <- as.data.frame(taxa_phyl_trim$asv_name)

#Export family dataframe with metadata
meta <- as.data.frame(reef[,1:70])
tr.fam.merge <- as.data.frame(t(family.merge))
fam.merge.meta <- as.data.frame(bind_cols(meta,tr.fam.merge))
write.csv(fam.merge.meta,"C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/KMreef_envfam_rar3000.csv")
