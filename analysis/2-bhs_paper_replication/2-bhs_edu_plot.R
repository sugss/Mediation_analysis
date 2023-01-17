
# BHS by age group

# We explore age and education-related gradients in the BHS and system-specific sub-scores in men and women separately. 
# For each age group and gender, we calculated the BHS values in the low, intermediate, and high education groups, 
# and tested for differences in these means using a Student’s T-test. 

# We also tested if these three values by age group and gender were supportive of a trend across education groups using a non-parametric Kruskal-Wallis rank test.

# we calculated the mean BHS for each age group a, gender g, and obtained three estimated mean BHS values: BHSa,g,L, BHSa,g,I and BHSa,g,H, for low, intermediate and high SEP groups, respectively. 
# These three values were compared in each age and gender category using a Student’s t-test and using the value in the high SEP group (BHSa,g,H) as a reference. 
# We also tested if these three values for each age group and gender were supportive of a trend across SEP groups using a non-parametric Kruskal-Wallis rank test. 
# These analyses were also performed on each system-specific subscore.


rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(dplyr)
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(broom)
library(tibble)
library(ggthemes)

# Set theme for ggplots
theme_set(theme_classic())


# Loading in data
bb = readRDS("../outputs/ukb_merged.rds")


####################################################

# Remove NA from education
bb = bb[complete.cases(bb[ , "education"]),]
# dropped 8,779 rows

# Reverse order of levels of education and sex
#bb$education <- factor(bb$education, levels=rev(levels(bb$education)))
bb$sex <- factor(bb$sex, levels=rev(levels(bb$sex)))

# Reverse order of levels of education
bb$education = factor(bb$education, levels=c("Low", "Intermediate", "High"))
label(bb$education) = "Education"


####################################################

####################################################
####################################################
# Plot of mean BHS and 95% CI
####################################################

# Create a dataset with mean and CI
bhs_means = summarySE(bb, measurevar="bhs", groupvars=c("education","age_recr", "sex"))

# Moves lines horizontally
pd <- position_dodge(0.1)

# Remove NAs
bhs_means = bhs_means[!is.na(bhs_means$education), ]

# BHS plot of mean and 95% CI
ggplot(bhs_means, aes(x=age_recr, y=bhs, colour=education)) + 
  geom_errorbar(aes(ymin=bhs-ci, ymax=bhs+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  xlab("Age group (years)") +
  ylab("Biological Health Score") +
  geom_text(aes(label=N), nudge_x=0.2, size=4) +
  facet_wrap(~sex) +
  scale_color_manual(values=c("blue", "green", "red")) +
  theme_classic()



# Check number of pple
bb %>%
  group_by(age_recr) %>%
  dplyr::summarise(n=n())

bb %>%
  dplyr::summarise(n=n())

# Saving extracted dataset
# saveRDS(bb, "../outputs/ukb_merged_2.rds")
