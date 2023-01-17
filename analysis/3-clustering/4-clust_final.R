
########################################  
# Clustering 


library(mclust)
library(ggplot2)
library(mltools)
library(data.table)
library(tibble)
library(dplyr)
library(hopkins)
library(labelled)


rm(list=ls())


# Loading in data
tt = readRDS("analysis/outputs/t2_scaled.rds")
mm = readRDS("analysis/outputs/mm.rds")



#########################################################
#########################################################
##### Hopkins
#########################################################
# Check if 1 cluster can be omitted
hopkins(tt, m=5000)
hopkins.pval(1, 5000)


#########################################################
#########################################################
##### Gaussian analysis
#########################################################
summary(mm)
mm$BIC
plot(mm, what = "BIC")

#########################################################
#########################################################
##### Add a cluster column onto dataset
#########################################################
tt_clust_g = data.frame(tt, cluster=mm$classification)

# Loading in data
bb = readRDS("analysis/outputs/ukb_merged.rds")
bb_label = bb


#########################################################
#########################################################
##### Add a cluster column onto main dataset
#########################################################
bb_col = tt_clust_g[,c(72,73)]
bbf = merge(bb, bb_col, by="row.names")
rownames(bbf)=bbf$Row.names
bbf = within(bbf, rm(Row.names, air_pm2.5.y))
names(bbf)[names(bbf) == "air_pm2.5.x"] = "air_pm2.5"
bbf = copy_labels(bb_label, bbf)
#########################################################

bb$cluster = as.factor(bb$cluster)

#####################################
# Saving updated dataset
saveRDS(bbf, "analysis/outputs/final/ukb_clust.rds")





#m3$classification
# or
# extract k and bic for each run

# concensus clustering M3C package - pack error metric


# plot(m3, what = c("classification"))
#plot(m3, what = "BIC")



########################################  
########################################  






