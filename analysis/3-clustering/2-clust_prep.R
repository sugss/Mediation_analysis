
########################################  
# Clustering 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(mclust)
library(ggplot2)
library(mltools)
library(data.table)
library(tibble)
library(dplyr)
library(hopkins)



# Loading in data
bb = readRDS("../outputs/ukb_merged.rds")
bb_label = bb


########################################  
########################################  
##### Prep data T 1,2,3
########################################  

# Create T 1 to 3 dataset
t1_vars = names(bb) %in% c('diet', 'dash', 'sleep', 'age_recr') 
t1 = bb[!t1_vars]
t1 = t1[,-c(1:6)]


# Remove NAs
t1 = na.omit(t1)   # drops 328,824 rows

# Create a copy just in case
t1_orig = t1

########################################  

# Save rownames as eid column
t1 = tibble::rownames_to_column(t1, "eid")
t1$eid = as.numeric(t1$eid)

# One-hot encode
t1 = one_hot(as.data.table(t1))

# Transform binary character vectors to numeric
t1$depression = ifelse(t1$depression=="Yes", 1, 0)
t1$pain_any = ifelse(t1$pain_any=="Yes", 1, 0)

# Save eid column as rownames
t1 = tibble::column_to_rownames(t1, "eid")

# Scale/standardise data
t1f = t1
t1f = scale(t1f)
t1f = as.data.frame(t1f)

#####################################
# Saving updated dataset
saveRDS(t1f, "../outputs/t1_scaled.rds")
#####################################
########################################  



########################################  
########################################  
##### Prep data T 2,3
########################################  

# Create T 1 to 3 dataset
t2_vars = names(bb) %in% c('sex', 'age_recr', 'breastfed', 'mom_smoke',
                           'diet', 'dash', 'sleep', 'age_recr_cont') 
t2 = bb[!t2_vars]
t2 = t2[,-c(1:6)]


# Remove NAs
t2 = na.omit(t2)   # drops 328,824 rows

# Create a copy just in case
t2_orig = t2

########################################  

# Save rownames as eid column
t2 = tibble::rownames_to_column(t2, "eid")
t2$eid = as.numeric(t2$eid)

# One-hot encode
t2 = one_hot(as.data.table(t2))

# Transform binary character vectors to numeric
t2$depression = ifelse(t2$depression=="Yes", 1, 0)
t2$pain_any = ifelse(t2$pain_any=="Yes", 1, 0)

# Save eid column as rownames
t2 = tibble::column_to_rownames(t2, "eid")

# Scale/standardise data
t2f = t2
t2f = scale(t2f)
t2f = as.data.frame(t2f)

#####################################
# Saving updated dataset
saveRDS(t2f, "../outputs/t2_scaled.rds")
#####################################
########################################  




########################################  
########################################  
##### Prep data T 3
########################################  

# Create T3 dataset
t3_vars = names(bb) %in% c('education', 
                           'sex', 'age_recr', 'breastfed', 'mom_smoke',
                           'diet', 'dash', 'sleep', 'age_recr_cont') 
t3 = bb[!t3_vars]
t3 = t3[,-c(1:6)]


# Remove NAs
t3 = na.omit(t3)   # drops 296,978 rows

# Create a copy just in case
t3_orig = t3

########################################  

# Save rownames as eid column
t3 = tibble::rownames_to_column(t3, "eid")
t3$eid = as.numeric(t3$eid)

# One-hot encode
t3 = one_hot(as.data.table(t3))

# Transform binary character vectors to numeric
t3$depression = ifelse(t3$depression=="Yes", 1, 0)
t3$pain_any = ifelse(t3$pain_any=="Yes", 1, 0)

# Save eid column as rownames
t3 = tibble::column_to_rownames(t3, "eid")

# Scale/standardise data
t3f = t3
t3f = scale(t3f)
t3f = as.data.frame(t3f)

#####################################
# Saving updated dataset
saveRDS(t3f, "../outputs/t3_scaled.rds")
#####################################
########################################  
