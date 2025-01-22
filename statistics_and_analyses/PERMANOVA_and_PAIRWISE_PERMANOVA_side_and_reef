#relationship between side & habitat across whole reef data set
adonis1 <- adonis(bc_reef~reef$Reef.Side*reef$Reef.Type)
adonis2 <- adonis(bc_reef~reef$total.Chl..ug.L.)
adonis3 <- adonis(bc_reef~reef$Nitrite.Nitrate..uM.)
adonis4 <- adonis(bc_surf~surf$Reef.Side*surf$Reef.Type)

#concatinate side and habitat
reef_type_side <- as.matrix(paste(reef$Reef.Side,reef$Reef.Type, sep = "_", collapse = NULL))
reef_type_side_surf <- as.matrix(paste(surf$Reef.Side,surf$Reef.Type, sep = "_", collapse = NULL))

#run pairwise adonis
pair.adonis1.1 <- pairwise.adonis(bc_reef,reef_type_side)
pair.adonis1.2 <- pairwise.adonis(bc_reef,reef$Reef.Side)
pair.adonis1.3 <- pairwise.adonis(bc_reef,reef$Reef.Type)
pair.adonis1.4 <- pairwise.adonis(bc_surf,reef_type_side_surf)
#write.csv(pair.adonis1.1, file="C:/Users/jacqu/Desktop/pairwise_adonis.csv")

#influence of reef env for each side
adonis2 <- adonis(bc_north~north$Reef.Type)
pair.adonis2 <- pairwise.adonis(bc_north, north$Reef.Type)

adonis3 <- adonis(bc_east~east$Reef.Type)
pair.adonis3 <- pairwise.adonis(bc_east, east$Reef.Type)

adonis4 <- adonis(bc_west~west$Reef.Type)
pair.adonis4 <- pairwise.adonis(bc_west, west$Reef.Type)

#run NMDS ordinations
surf.nmds = metaMDS(norm_surf, distance = "bray", trymax=100) #stress= 0.12
reef.nmds = metaMDS(norm_reef, distance = "bray", trymax=100) #stress= 0.13
