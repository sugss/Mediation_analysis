
# Cox

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(sjlabelled)
library(survival)
library(gtsummary)
library(finalfit)
library(ggplot2)
library(jtools)
library(ggpubr)


# Loading in data
ukb_cov = readRDS("../outputs/ukb_merged.rds")
bb = ukb_cov

# cvd = readRDS("../../diseases/hesin_extraction/outputs/cvd/output_final.rds")
cancer = readRDS("../../diseases/hesin_extraction/outputs/cancer/output_final.rds")


##################################################
##################################################
# Cancer incidence
##################################################
# Set a censoring date
date_censor = as.Date("2016-03-01")

# Create required columns
cancer = cancer %>%
  mutate(time = ifelse(incident_case==1 & date_diagnosis<=date_censor, date_diagnosis-date_recr, 
                       ifelse(incident_case==1 & date_diagnosis>date_censor, ifelse(!is.na(date_death) & date_death<=date_censor, date_death-date_recr, date_censor-date_recr), 
                              ifelse(!is.na(date_death) & date_death<=date_censor, date_death-date_recr, date_censor-date_recr)))) %>%
  mutate(status = ifelse(incident_case==1 & date_diagnosis<=date_censor, 1, 0))

# Extract required columns                         
cancer_i = cancer[, c("eid" ,"status", "time")]

# Merge onto main dataset
bb = merge(bb, cancer_i, by.x="row.names", by.y="eid")
rownames(bb)=bb$Row.names
bb = within(bb, rm(Row.names))
#names(bb)[names(bb) == 'time_to_diagnosis'] <- 'time'

bb$age = with(bb, age_recr_cont*365.25)

# Scale scores for HRs
bb$bhs = bb$bhs/0.1  
bb$met_sys = bb$met_sys/0.1
bb$car_sys = bb$car_sys/0.1
bb$inf_sys = bb$inf_sys/0.1
bb$liv_fun = bb$liv_fun/0.1
bb$kid_fun = bb$kid_fun/0.1
# thus 1 unit change == 0.1 unit change now

# Separate sexes
bb_male = bb[bb$sex=="Male", ]
bb_female = bb[bb$sex=="Female", ]

# Restore labels from original data
bb_male = copy_labels(bb_male, ukb_cov)
bb_female = copy_labels(bb_female, ukb_cov)


##################################################
##################################################
# Cox model - BHS

m1 = coxph(Surv(age, age+time, status) ~ bhs, bb_male)
m2 = coxph(Surv(age, age+time, status) ~ bhs + 
             education, bb_male)
m3 = coxph(Surv(age, age+time, status) ~ bhs + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_male)
m4 = coxph(Surv(age, age+time, status) ~ bhs + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_male)
m5 = coxph(Surv(age, age+time, status) ~ bhs + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_male)
p1 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c("BHS" = "bhs"),
                colors = "Reds",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  rremove("ylab") +
  ggtitle("Men") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y = element_text(angle = 90, face="bold"),
        axis.text.x = element_blank(),
        legend.position="none")

m1 = coxph(Surv(age, age+time, status) ~ bhs, bb_female)
m2 = coxph(Surv(age, age+time, status) ~ bhs + 
             education, bb_female)
m3 = coxph(Surv(age, age+time, status) ~ bhs + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_female)
m4 = coxph(Surv(age, age+time, status) ~ bhs + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_female)
m5 = coxph(Surv(age, age+time, status) ~ bhs + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_female)
p2 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c(" " = "bhs"),
                colors = "Reds",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  ggtitle("Women") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank())

##################################################
##################################################
# Cox model - metabolic
m1 = coxph(Surv(age, age+time, status) ~ met_sys, bb_male)
m2 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education, bb_male)
m3 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_male)
m4 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_male)
m5 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_male)
p3 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c("Metabolic" = "met_sys"),
                colors = "Blues",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank(),
        legend.position="none")

m1 = coxph(Surv(age, age+time, status) ~ met_sys, bb_female)
m2 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education, bb_female)
m3 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_female)
m4 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_female)
m5 = coxph(Surv(age, age+time, status) ~ met_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_female)
p4 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c(" " = "met_sys"),
                colors = "Blues",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank())

##################################################
##################################################
# Cox model - cardiovascular
m1 = coxph(Surv(age, age+time, status) ~ car_sys, bb_male)
m2 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education, bb_male)
m3 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_male)
m4 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_male)
m5 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_male)
p5 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c("Cardiovascular" = "car_sys"),
                colors = "Greens",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank(),
        legend.position="none")

m1 = coxph(Surv(age, age+time, status) ~ car_sys, bb_female)
m2 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education, bb_female)
m3 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_female)
m4 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_female)
m5 = coxph(Surv(age, age+time, status) ~ car_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_female)
p6 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c(" " = "car_sys"),
                colors = "Greens",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank())

##################################################
##################################################
# Cox model - inflammatory
m1 = coxph(Surv(age, age+time, status) ~ inf_sys, bb_male)
m2 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education, bb_male)
m3 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_male)
m4 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_male)
m5 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_male)
p7 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c("Inflammatory" = "inf_sys"),
                colors = "Purples",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank(),
        legend.position="none")

m1 = coxph(Surv(age, age+time, status) ~ inf_sys, bb_female)
m2 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education, bb_female)
m3 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_female)
m4 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_female)
m5 = coxph(Surv(age, age+time, status) ~ inf_sys + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_female)
p8 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c(" " = "inf_sys"),
                colors = "Purples",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank())

##################################################
##################################################
# Cox model - liver
m1 = coxph(Surv(age, age+time, status) ~ liv_fun, bb_male)
m2 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education, bb_male)
m3 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_male)
m4 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_male)
m5 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_male)
p9 = plot_summs(m1, m2, m3, m4, m5,
                coefs= c("Liver" = "liv_fun"),
                colors = "Oranges",
                exp = TRUE,
                point.shape = FALSE,
                point.size = 1,
                scale = TRUE,
                legend.title = "",
                model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank(),
        legend.position="none")

m1 = coxph(Surv(age, age+time, status) ~ liv_fun, bb_female)
m2 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education, bb_female)
m3 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_female)
m4 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_female)
m5 = coxph(Surv(age, age+time, status) ~ liv_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_female)
p10 = plot_summs(m1, m2, m3, m4, m5,
                 coefs= c(" " = "liv_fun"),
                 colors = "Oranges",
                 exp = TRUE,
                 point.shape = FALSE,
                 point.size = 1,
                 scale = TRUE,
                 legend.title = "",
                 model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        axis.text.x = element_blank())

##################################################
##################################################
# Cox model - kidney
m1 = coxph(Surv(age, age+time, status) ~ kid_fun, bb_male)
m2 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education, bb_male)
m3 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_male)
m4 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_male)
m5 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_male)
p11 = plot_summs(m1, m2, m3, m4, m5,
                 coefs= c("Kidney" = "kid_fun"),
                 colors = "Greys",
                 exp = TRUE,
                 point.shape = FALSE,
                 point.size = 1,
                 scale = TRUE,
                 legend.title = "",
                 model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  # labs(x = "\n HR \n ", y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"),
        legend.position="none")

m1 = coxph(Surv(age, age+time, status) ~ kid_fun, bb_female)
m2 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education, bb_female)
m3 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education + 
             smoke_status + alc_freq + phys_activ, bb_female)
m4 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi, bb_female)
m5 = coxph(Surv(age, age+time, status) ~ kid_fun + 
             education + 
             smoke_status + alc_freq + phys_activ +
             bmi +
             meds_num + comorb, bb_female)
p12 = plot_summs(m1, m2, m3, m4, m5,
                 coefs= c(" " = "kid_fun"),
                 colors = "Greys",
                 exp = TRUE,
                 point.shape = FALSE,
                 point.size = 1,
                 scale = TRUE,
                 legend.title = "",
                 model.names = c("Unadjusted", "+ Education", "+ Behaviours", "+ BMI", "+ Medical")) +
  labs(x = NULL, y = NULL) +
  xlim(0.95, 1.05) +
  theme(panel.grid.major.x = element_line(linetype = 1),
        panel.grid.minor.x = element_line(linetype = 1),
        axis.text.y=element_text(angle = 90, face="bold"))


##################################################
##################################################
p = ggarrange(p1, p2,
              p3, p4,
              p5, p6,
              p7, p8,
              p9, p10,
              p11, p12,
              ncol = 2, nrow = 6,
              align = "v" )

annotate_figure(p, 
                top = text_grob("Cancer Incidence", face = "bold"),
                bottom = text_grob("HR", face = "bold"))
##################################################
##################################################
