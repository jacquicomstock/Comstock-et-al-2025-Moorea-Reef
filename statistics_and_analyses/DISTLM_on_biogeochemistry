#import data
mydata <- read.csv("C:/Users/jacqu/Desktop/Research/Projects/Moorea/KM Reef/Reef_Analyses/KM_envasv_surfpel_reef_rar3000_nmds.csv", header=TRUE,sep=",",row.names=1)

#filter out just upper reef samples
surf <- filter(mydata, Broad_Location == "Reef")

#isolate variable you want to use for PERMANOVA from the overall dataframe
group <- surf$total.Chl..ug.L.
group <- surf$density..kg.m3.
group <- surf$DOC..uM.C.
group <- surf$Nitrite.Nitrate..uM.
group <- surf$POC..umol.L.
group <- surf$PP..mg.m3.day.

#isolate ASV columns from metadata
com = surf[,71:ncol(surf)]
m_com = as.matrix(com)

#Run PERMANOVA
adonis(m_com ~ group, permutations=999)
