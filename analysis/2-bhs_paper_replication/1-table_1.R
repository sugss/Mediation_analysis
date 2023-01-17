
# Table 1

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(table1)
library(tidyverse)


# Loading in data
bb = readRDS("../outputs/ukb_merged.rds")
bb_label = bb


# Reverse order of levels of education
bb$education = factor(bb$education, levels=c("Low", "Intermediate", "High"))
label(bb$education) = "Education"

# Table 1
table1(~ ethnicity + education + employ +
         accom_type + accom_own + accom_income + accom_pple +
         
         bmi + comorb + meds_num + vit_num + 
         vit_d + depression + pain_any +
         
         smoke_status + packyears + alcohol_status + alc_freq +
         drink_water + phys_activ + walk_days_pw + 
         sleep + sun_protect +
         
         depriv_index + pop_density + dist_coast + 
         major_road + noise_night +
         air_nox + air_pm10 + air_pm2.5_abs + air_pm2.5 +
         
         breastfed + mom_smoke
       
         | sex * age_recr , 
       
       render.continuous=c(.="N", .="Mean (SD)") , big.mark=",",
       data=bb, overall = FALSE)



# Table 1 - similar to paper
table1(~ education + 
         meds_num +
         smoke_status + 
         phys_activ + 
         alc_freq +
         bmi
       | sex * age_recr , 
       
       render.continuous=c(.="N", .="Mean (SD)") ,  big.mark=",",
       data=bb, overall = FALSE)




