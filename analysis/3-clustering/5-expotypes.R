

rm(list=ls())


library(table1)
library(tidyverse)


# Loading in data
bb = readRDS("analysis/outputs/ukb_clust_g.rds")
bb_label = bb



ff = aggregate(bb, by=list(cluster=bb$cluster), mean)
f = aggregate(bb, by=list(cluster=bb$cluster), length)





# Table 1
table1(~ ethnicity + education + employ +
         accom_type + accom_own + accom_income + accom_pple +
         
         bmi + comorb + meds_num + vit_num + 
         vit_d + depression + pain_any +
         
         smoke_status + packyears + alcohol_status + alc_freq +
         drink_water + phys_activ + walk_days_pw + 
         sleep + sun_protect +
         
         depriv_index + pop_density + dist_coast + 
         noise_night +
         air_nox + air_pm10 + air_pm2.5_abs + air_pm2.5 +
         
         breastfed + mom_smoke
       
       | cluster , 
       
       render.continuous=c(.="Mean (SD)") , big.mark=",",
       data=bb, overall = FALSE)








