
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

########################################  
########################################  
##### Gaussian clustering
########################################  

# Hopkins to check if 1 cluster can be omitted
hopkins(tt, m=5000)
hopkins.pval(1, 5000)


m1 = Mclust(tt, G=2:10)
summary(m1)
m1$BIC
plot(m1, what = "BIC")
# VII, 10 highest

m2 = Mclust(tt, G=10:20)
summary(m2)
m2$BIC
plot(m2, what = "BIC")
# VII 15 highest + EII 20

m3 = Mclust(tt, G=20:30)
summary(m3)
m3$BIC
plot(m3, what = "BIC")
# EII 30 highest

m4 = Mclust(tt, G=30:40)
summary(m4)
m4$BIC
plot(m4, what = "BIC")
# EII 40 highest

m5 = Mclust(tt, G=40:50)
summary(m5)
m5$BIC
plot(m5, what = "BIC")
# EII 50 highest

m6 = Mclust(tt, G=50:60)
summary(m6)
m6$BIC
plot(m6, what = "BIC")

m7 = Mclust(tt, G=60:70)
summary(m7)
m7$BIC
plot(m7, what = "BIC")

m8 = Mclust(tt, G=70:80)
summary(m8)
m8$BIC
plot(m8, what = "BIC")

#########################################################
#########################################################
##### Add a cluster column onto dataset
#########################################################
tt_clust_g = data.frame(tt, cluster=m2$classification)

# Loading in data
bb = readRDS("analysis/outputs/ukb_merged.rds")
bb_label = bb


#########################################################
#########################################################
##### Add a column from diet onto main dataset
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
saveRDS(bbf, "analysis/outputs/ukb_clust_g.rds")





#m3$classification
# or
# extract k and bic for each run

# concensus clustering M3C package - pack error metric


# plot(m3, what = c("classification"))
#plot(m3, what = "BIC")



########################################  
########################################  






