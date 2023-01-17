
# Mediation with Baron + Kenny
# https://benwhalley.github.io/just-enough-r/mediation.html

rm(list=ls())

library(dplyr)
library(pander)
library(broom)
library(mediation)


# Loading in data
bb = readRDS("analysis/outputs/mediation.rds")
bb_label = bb

###################################################
# S1: Do covs predict CVD?
###################################################
s1_1 = glm(cvd ~ sex + age_recr + ethnicity + breastfed + mom_smoke, data=bb, family="binomial")
tidy(s1_1) %>% pander()
# 

s1_2 = glm(cvd ~ education, data=bb, family="binomial")
tidy(s1_2) %>% pander()
# coeff for covs is stat significant thus YES

s1_3 = glm(cvd ~ cluster, data=bb, family="binomial")
tidy(s1_3) %>% pander()
# coeff for covs is stat significant thus YES


###################################################
# S2: Do covs predict BHS?
###################################################
s2_1 = lm(bhs ~ sex + age_recr + ethnicity + breastfed + mom_smoke, data=bb)
tidy(s2_1) %>% pander()
# 

s2_2 = lm(bhs ~ education, data=bb)
tidy(s2_2) %>% pander()
# coeff for covs is stat significant thus YES

s2_3 = lm(bhs ~ cluster, data=bb)
tidy(s2_3) %>% pander()
# coeff for covs is stat significant thus YES


###################################################
# S3: Does BHS predict CVD, controlling for covs?
###################################################
s3_1 = glm(cvd ~ sex + age_recr + ethnicity + breastfed + mom_smoke + bhs, data=bb, family="binomial")
tidy(s3_1, conf.int = T) %>% pander()
# 
# 

s3_2 = glm(cvd ~ education + bhs, data=bb, family="binomial")
tidy(s3_2, conf.int = T) %>% pander()
# coeff for BHS is stat significant thus YES
# mediation occurs

s3_3 = glm(cvd ~ cluster + bhs, data=bb, family="binomial")
tidy(s3_3, conf.int = T) %>% pander()
# coeff for BHS is stat significant thus YES
# mediation occurs


###################################################
# S4: Does covs predict CVD, controlling for BHS?
###################################################
# coeff for covs is stat significant thus YES
# mediation is partial, according to B+K


###### Attention!!
# If predictor and mediator are measured with error (noisy measures), 
# then the proportion of the effect which appears to be mediated 
# will be reduced artificially!



###################################################
###################################################
###################################################

## Non-parametric bootstrap

# Function accepts the 2nd and 3rd regression models from the ‘Baron and Kenny’ steps, along with arguments which identify which variables are the predictor and the mediator.
# From this, function calculates the indirect effect, and the proportion of the total effect mediated. 
# This is accompanied by a bootstrapped standard-error, and associated p-value.

set.seed(1234)
clust_med_3 = mediation::mediate(s2_3, s3_3, treat = "cluster", mediator = "bhs")
summary(clust_med_3)
plot(clust_med_3)
# Indirect effect is statistically significant
# around hald of the total effect is mediated via BHS 
# Because covs in iteself is not a plausable cause of CVD, this suggest that other factors might be important in mediating this residual direct effect

### ACME - average causal mediation effect == S2*S3
# H0: a*b=0
# conf int does not contain 0 & p-value is tiny
# reject H0, thus evidence that effect of cluster on CVD is mediated by BHS

### ADE - average direct effect == S3
# effect of cluster that is not mediated
# conf int does not contain 0
# thus mediation is partial

### Total effect - total effect of cluster on CVD (ACME+ADE) == S1
# ??very little of total effect is mediated via BHS

### Prop.mediated - prop of effect that is mediated (ACME/Total effect)
# On average, only 24.3% of the effect of cluster on CVD is mediated by BHS
# thus residual direct effect if quite substantial


#### Conclusion
# The effect of cluster on CVD was fully mediated via tBHS
# Regression coefficient between cluster and CVD and the regression coefficient between BHS and CVD were significant. 
# The average indirect effect was (0.0017)*(0.0090) = 0.0002. 
# We tested the significance of this indirect effect using bootstrapping procedures. Unstandardised indirect effects were computed for each of 1’000 bootstrapped samples, and the 95% confidence interval was computed by determining the indirect effects at the 2.5th and 97.5th percentiles. 
# The bootstrapped unstandardised indirect effect was 0.0002, and the 95% confidence interval ranged from 0.000199 to 0.00. 
# Thus, the indirect effect was statistically significant (p<.001).






