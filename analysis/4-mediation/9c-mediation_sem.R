
# Mediation with Path models
# https://benwhalley.github.io/just-enough-r/mediation-using-path-models.html

rm(list=ls())

library(dplyr)
library(pander)
library(broom)


# Loading in data
bb = readRDS("analysis/outputs/mediation.rds")
bb_label = bb

# Remove NAs ????????
#bb = na.omit(bb)   # drops 31,846 rows

library(piecewiseSEM)

m1_m = lm(bhs ~ sex + age_recr + ethnicity + breastfed + mom_smoke, data=bb)
m1_y = glm(cvd ~ sex + age_recr + ethnicity + breastfed + mom_smoke + bhs, data=bb, family="binomial")

m1_result = psem(m1_m, m1_y, data=bb)
summary(m1_result)
plot(m1_result)

###############################
###############################

m2_m = lm(bhs ~ education, data=bb)
m2_y = glm(cvd ~ education + bhs, data=bb, family="binomial")

m2_result = psem(m2_m, m2_y, data=bb)
summary(m2_result)
plot(m2_result)

###############################
###############################

m3_m = lm(bhs ~ cluster, data=bb)
m3_y = glm(cvd ~ cluster + bhs, data=bb, family="binomial")

m3_result = psem(m3_m, m3_y, data=bb)
summary(m3_result)
plot(m3_result)

###############################
###############################






library(lavaan)

m3 = '
  cvd ~ B*bhs + C*cluster
  bhs ~ A*cluster

  # computed parameters, see http://lavaan.ugent.be/tutorial/mediation.html
  indirect := A*B
  total := C + (A*B)
  proportion := indirect/total
'

bb$cvd = ordered(bb$cvd)
m3_fit = sem(m3, data=bb)
summary(m3_fit)

set.seed(1234)
m3_fit_boot <- sem(m3, data=bb, test="bootstrap", bootstrap=100)

parameterEstimates(m3_fit_boot) %>%
  filter(op == ":=") %>%
#  select(label, est, pvalue) %>%
  pander::pander()




############

m2 = '
  cvd ~ B*bhs + C*education
  bhs ~ A*education

  # computed parameters, see http://lavaan.ugent.be/tutorial/mediation.html
  indirect := A*B
  total := C + (A*B)
  proportion := indirect/total
'

bb$cvd = ordered(bb$cvd)
m2_fit = sem(m2, data=bb)
summary(m2_fit)







