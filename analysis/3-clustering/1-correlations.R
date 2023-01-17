

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(psych)
library(dplyr)
library(ggcorrplot)


# Loading in data
bb = readRDS("../outputs/ukb_merged.rds")
bb_label = bb



# Create T1 dataset
t1_vars = names(bb) %in% c('sex', 'age_recr', 'ethnicity', 'mom_smoke', 'breastfed') 
t1 = bb[t1_vars]

# Create T2 dataset
t2 = bb['education']

# Create T3 dataset
t3_vars = names(bb) %in% c('bhs', 'met_sys', 'car_sys', 'inf_sys', 'liv_fun', 'kid_fun', 
                           'sex', 'age_recr', 'ethnicity', 'mom_smoke', 'breastfed', 'education', 
                           'age_recr_cont', 'sleep_cat', 'diet_cat', 'dash') 
t3 = bb[!t3_vars]




model.matrix(~0+., data=t1) %>% 
  cor(use="everything") %>% 
  # NAs will propagate conceptually, a resulting value will be NA whenever one of its contributing observations is NA
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)



model.matrix(~0+., data=t3) %>% 
  cor(use="everything") %>%  
  # does not diregard NA rows, uses the non-NA values when calculating the correlation
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)








