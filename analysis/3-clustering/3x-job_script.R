
########################################  
# Clustering 


library(mclust)
library(ggplot2)
library(mltools)
library(data.table)
library(tibble)
library(dplyr)
library(labelled)
#library(hopkins)


# Reading arguments
args = commandArgs(trailingOnly = TRUE)
data_path = as.character(args[1])


# Loading in data
tt = readRDS(data_path)

########################################  
########################################  
##### Gaussian clustering
########################################  

# Hopkins to check if 1 cluster can be omitted
#hopkins(tt, m=5000)
#hopkins.pval(1, 5000)


mm = Mclust(tt, G=1:20)
print(summary(mm))
print(mm$BIC)

#pdf("clust_plot.pdf")
#plot(mm, what = "BIC")


#plot(mm, what = c("classification"))



#########################################################
#########################################################
##### Add a cluster column onto dataset
#########################################################
#tt_clust_g = data.frame(tt, cluster=m2$classification)

# Loading in data
#bb = readRDS("analysis/outputs/ukb_merged.rds")
#bb_label = bb


#########################################################
#########################################################
##### Add a column from diet onto main dataset
#########################################################
#bb_col = tt_clust_g[,c(72,73)]
#bbf = merge(bb, bb_col, by="row.names")
#rownames(bbf)=bbf$Row.names
#bbf = within(bbf, rm(Row.names, air_pm2.5.y))
#names(bbf)[names(bbf) == "air_pm2.5.x"] = "air_pm2.5"
#bbf = copy_labels(bb_label, bbf)
#########################################################

#bb$cluster = as.factor(bb$cluster)

#####################################
# Saving updated dataset
#saveRDS(bbf, "analysis/outputs/ukb_clust_g.rds")






