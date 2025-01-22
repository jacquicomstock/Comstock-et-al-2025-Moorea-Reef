#relabund.df = dataframe containing ONLY relative abundance data, no metadata or other info. Samples in rows and OTUs in columns.
relabund.df <- as.matrix(m_reef/100)
#min.num = the minimum number of samples an OTU needs to be present in to not be culled.
min.num <- 10
#min.abund = the minimum relative abundance an OTU needs to have in (the min.num) samples to not be culled.
min.abund <- .0001
#min.single.abund = the minimum relative abundance an OTU needs to have in a SINGLE sample to not be culled.
min.single.abund <-.03

  
#create the function
cull.asv <- function(relabund.df, min.num, min.abund, min.single.abund) {
  #create a empty vector
  sub=c()
  #make a function that says for any input, generate a logical vector of TRUEs and FALSEs that will be used for subsetting, selecting OTUs that
  cull=function(x) {
  #have a relabund>"min.abund" in "min.num" samples
  sub=ifelse(length(x[x>=min.abund])>=min.num 
  #or have a relabund>"min.single.abund" in at least one sample           
  | length(x[x>=min.single.abund])>0,TRUE,FALSE) 
    return(sub)}
  #apply cull function to relabund.df, save output as a vector.
  cull.vec=apply(relabund.df,2,FUN=cull)
  #Use cull.vec to subset the columns of relabund.df for OTUs that passed the cull threshold.
  relabund.df.cull=relabund.df[,cull.vec]
  relabund.df.cull<<-relabund.df.cull
}

#run the function on ASVs
Reef_asv_cull <- as.matrix(cull.asv(relabund.df, min.num, min.abund, min.single.abund))
norm_reef_asvcull<-asin(sqrt(Reef_asv_cull))

#calculate averages
reef.avg <- t(aggregate(x= Reef_asv_cull, by = list(reef$Reef.Side.Habitat), FUN = mean))
#calculate st dev
reef.stdev <- t(aggregate(x= Reef_asv_cull, by = list(reef$Reef.Side.Habitat), FUN = sd))

#combine the two data frames
asv.avg.stdev <- as.data.frame(cbind(reef.avg, reef.stdev))
write.csv(asv.avg.stdev, "C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/Reef_ASVcull_10s_avg.csv")


#run function on families
relabund.df.fam <- as.matrix(t(family.merge)/100)
Reef_fam_cull <- as.matrix(cull.asv(relabund.df.fam, min.num, min.abund, min.single.abund))
norm_reef_famcull<-asin(sqrt(Reef_fam_cull))

#calculate averages for families
famreef.avg <- t(aggregate(x= Reef_fam_cull, by = list(fam.merge.meta$Reef.Side.Habitat), FUN = mean))
#calculate st dev for families
famreef.stdev <- t(aggregate(x= Reef_fam_cull, by = list(fam.merge.meta$Reef.Side.Habitat), FUN = sd))
#combine 2 dataframes
fam.avg.stdev <- as.data.frame(cbind(famreef.avg, famreef.stdev))

write.csv(fam.avg.stdev, "C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/Reef_famcull_10s_avg.csv")
