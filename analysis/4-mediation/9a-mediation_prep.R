

rm(list=ls())


library(tidyverse)
library(labelled)
library(Hmisc)



# Loading in data
bb = readRDS("analysis/outputs/ukb_clust_g.rds")
bb_label = bb

# Drop useless vars
bb = bb[ ,-c(11:43)]

# Loading in diseases
cvd = readRDS("diseases/hesin_extraction/outputs/cvd/output_final.rds")
cvd$cvd = as.factor(cvd$incident_case)

# Merge everything together
bb2 = merge(bb, cvd[,c(1,11)], by.x="row.names", by.y="eid")
rownames(bb2)=bb2$Row.names

# Remove unnecessary column
bb2 = within(bb2, rm(Row.names))

# Reorder columns
bb2 = bb2 %>%
  relocate( sex, age_recr, ethnicity, breastfed, mom_smoke,
    
            education, 
            
            cluster,
            
            bhs, met_sys, car_sys, inf_sys, liv_fun, kid_fun,
            
            cvd )

# Remove unnecessary column
bb2 = within(bb2, rm(age_recr_cont))

# Relabel the new data
bb2 = copy_labels(bb2, bb_label)
label(bb2$cluster) = "T3 cluster"
label(bb2$cvd) = "CVD incidence"


# Saving extracted dataset
saveRDS(bb2, "analysis/outputs/mediation.rds")




