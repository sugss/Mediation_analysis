
# Combine datasets

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(openxlsx)
library(tibble)
library(sjlabelled)
library(Hmisc)
library(tidyverse)
library(finalfit)
library(labelled)



# Loading in bhs and covariates
ukb_bhs = readRDS("../bhs/extraction_and_recoding/outputs/ukb_bhs.rds")
ukb_cov2 = readRDS("../covariates/extraction_and_recoding/outputs/ukb_final_cov.rds")
ukb_cov = ukb_cov2

# Loading in diseases
cvd = readRDS("../diseases/hesin_extraction/outputs/cvd/output_final_cut.rds")
cancer = readRDS("../diseases/hesin_extraction/outputs/cancer/output_final_cut.rds")
# table(cvd$cvd, cancer$cancer, useNA ="always")


#########################################################
# Loading in comorbidities
c1 = readRDS("../diseases/hesin_extraction/outputs/asthma/output_final_cut.rds")
c2 = readRDS("../diseases/hesin_extraction/outputs/arthritis/output_final_cut.rds")
c3 = readRDS("../diseases/hesin_extraction/outputs/emphysema/output_final_cut.rds")
c4 = readRDS("../diseases/hesin_extraction/outputs/hyperthyroidism/output_final_cut.rds")
c5 = readRDS("../diseases/hesin_extraction/outputs/hypothyroidism/output_final_cut.rds")
c6 = readRDS("../diseases/hesin_extraction/outputs/bronchitis/output_final_cut.rds")
c7 = readRDS("../diseases/hesin_extraction/outputs/liver/output_final_cut.rds")
c8 = readRDS("../diseases/hesin_extraction/outputs/diabetes/output_final_cut.rds")
c9 = readRDS("../diseases/hesin_extraction/outputs/epilepsy/output_final_cut.rds")
c10 = readRDS("../diseases/hesin_extraction/outputs/depression/output_final_cut.rds")
c11 = readRDS("../diseases/hesin_extraction/outputs/kidney/output_final_cut.rds")
comorb = list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11)

# Merge comorbidities
comorb = comorb %>% 
  reduce(full_join, by='eid')

# Turn diseases to factors
comorb = comorb %>% 
  mutate_if(is.numeric, as.factor)

# Create a combo comorbidities column
comorb = comorb %>%
  rowwise %>%
  mutate(comorb = ifelse(rowSums(across(asthma:kidney, ~ .x == "1"), na.rm = T)>0, "One or more", "None"))

comorb$comorb = as.factor(comorb$comorb)

#ukb_cov = rename(ukb_cov, depression_temp = depression)
#ukb_cov2 = rename(ukb_cov2, depression_temp = depression)


#########################################################
# Merge comorb and cov
ukb_cov = merge(ukb_cov, comorb, by.y="eid", by.x="row.names")
rownames(ukb_cov)=ukb_cov$Row.names
ukb_cov = ukb_cov[ ,-c(1, 34:44)]
ukb_cov = copy_labels(ukb_cov, ukb_cov2)

# Merge bhs and cov
bb = merge(ukb_bhs, ukb_cov, by="row.names")
rownames(bb)=bb$Row.names
bb = bb[ ,2:length(bb)]
bb = copy_labels(bb, ukb_cov2)

# Merge all med covs
cc = merge(cvd, cancer, by="eid")

# Turn diseases to factors
cc = cc %>% 
  mutate_if(is.numeric, as.factor)

# Merge everything together
bb = merge(bb, cc, by.x="row.names", by.y="eid")
rownames(bb)=bb$Row.names
# Remove unnecessary column
bb = within(bb, rm(Row.names))


#########################################################
# Loading in depression
depress = readRDS("../diseases/hesin_extraction/outputs/depression_full/output_final_cut.rds")

# Merge ukb and depression
bb2 = merge(bb, depress, by.y="eid", by.x="row.names")
rownames(bb2)=bb2$Row.names
bb2 = bb2[ ,2:length(bb2)]
bb2 = copy_labels(bb2, ukb_cov2)

# Create a depression variable
bb2 = bb2 %>% 
  mutate(depression = if_else(depression_temp=="Yes" | depression_full=="1", "Yes", "No") %>% 
           ff_label("Depression"))



#########################################################
# DROP PREVALENT CASES
bb3 = subset(bb2, cancer!=1 & cvd!=1)
#########################################################

# Drop useless vars
bb3 = within(bb3, rm(cvd,cancer,depression_temp,depression_full,major_road))

# Restore labels from original data
bb3 = copy_labels(bb3, ukb_cov2)

# Label diseases and other vars
label(bb3$sex) = "Gender"
label(bb3$age_recr) = "Age group"
label(bb3$age_recr_cont) = "Age"
label(bb3$met_sys) = "Metabolic system"
label(bb3$car_sys) = "Cardiovascular system"
label(bb3$inf_sys) = "Inflammatory system"
label(bb3$liv_fun) = "Liver function"
label(bb3$kid_fun) = "Kidney function"
label(bb3$bhs) = "BHS"
label(bb3$comorb) = "Comorbidities"
label(bb3$depression) = "Depression"
# levels(bb$cvd) = c("non-case"="0", "case"="1")
# levels(bb$cancer) = c("non-case"="0", "case"="1")

# Rename column
# names(bb2)[names(bb2) == "v_activ_days_pw"] = "phys_activ"



#########################################################
#########################################################
##### Add a column from diet onto main dataset
#########################################################
bb_col = readRDS("../diet/extraction_and_recoding/outputs/ukb_recoded_diet.rds")
bb_col = bb_col[,c(20,21)]
bbf = merge(bb3, bb_col, by="row.names")
rownames(bbf)=bbf$Row.names
bbf = within(bbf, rm(Row.names))
bbf = copy_labels(bb3, bbf)
#########################################################
#########################################################



  
# Reorder columns
bbf = bbf %>%
  relocate( bhs, met_sys, car_sys, inf_sys, liv_fun, kid_fun,
            sex, age_recr,
            
            ethnicity, education, employ,
            
            accom_type, accom_own, accom_income, accom_pple,
            
            bmi, comorb, meds_num, 
            vit_num, vit_d, depression, pain_any,
            
            packyears, smoke_status, alcohol_status, alc_freq,
            drink_water, diet, diet_cat,
            phys_activ, walk_days_pw, 
            sleep, sleep_cat, sun_protect,
            
            depriv_index, pop_density, dist_coast, noise_night,
            air_nox, air_pm10, air_pm2.5_abs, air_pm2.5, 
            
            breastfed, mom_smoke)



# Saving extracted dataset
saveRDS(bbf, "../analysis/outputs/ukb_merged.rds")



# Create unordered dataset
#unorder = function(x) {factor(x, ordered=FALSE)}
#bb3 = bb %>% 
#  mutate_if(is.ordered, unorder)
#bb3 = bb3 %>% copy_labels_from(bb)


# Saving extracted dataset
#saveRDS(bb3, "analysis/outputs/ukb_merged_unordered.rds")










