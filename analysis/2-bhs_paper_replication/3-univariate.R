
# Univariate analysis

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(gtsummary)
# https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html
library(labelled)
library(dplyr)
library(purrr)


# Loading in data
bb2 = readRDS("../outputs/ukb_merged.rds")


# Univariate - bhs
t1 = bb2 %>%
  select(!(c('sex','age_recr', 'age_recr_cont',
            'met_sys', 'car_sys', 'inf_sys', 'liv_fun', 'kid_fun'))) %>%
  tbl_uvregression(
    method = lm,
    y = bhs,
    estimate_fun = ~style_sigfig(.x, digits = 3),
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    conf.int = FALSE 
    ) %>%
#  add_global_p(keep=TRUE) %>%  # uses car::Anova(type=III)
  bold_p(t=0.05) %>%
  italicize_levels() %>%
  bold_labels() %>%
  modify_caption("**Univariate analysis of BHS as exposure**") %>%
  modify_header(label = "**Variable**")


# Univariate - met_sys
t2 = bb2 %>%
  select(!(c('sex','age_recr', 'age_recr_cont',
             'bhs', 'car_sys', 'inf_sys', 'liv_fun', 'kid_fun'))) %>%
  tbl_uvregression(
    method = lm,
    y = met_sys,
    estimate_fun = ~style_sigfig(.x, digits = 3),
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    hide_n = TRUE,
    conf.int = FALSE 
  ) %>%
  #  add_global_p(keep=TRUE) %>%  # uses car::Anova(type=III)
  bold_p(t=0.05) %>%
  italicize_levels() %>%
  bold_labels() %>%
  modify_caption("**Univariate analysis of Metabolic system as exposure**") %>%
  modify_header(label = "**Variable**")


# Univariate - car_sys
t3 = bb2 %>%
  select(!(c('sex','age_recr', 'age_recr_cont',
             'bhs', 'met_sys', 'inf_sys', 'liv_fun', 'kid_fun'))) %>%
  tbl_uvregression(
    method = lm,
    y = car_sys,
    estimate_fun = ~style_sigfig(.x, digits = 3),
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    hide_n = TRUE,
    conf.int = FALSE 
  ) %>%
  #  add_global_p(keep=TRUE) %>%  # uses car::Anova(type=III)
  bold_p(t=0.05) %>%
  italicize_levels() %>%
  bold_labels() %>%
  modify_caption("**Univariate analysis of Cardiovascular system as exposure**") %>%
  modify_header(label = "**Variable**")


# Univariate - inf_sys
t4 = bb2 %>%
  select(!(c('sex','age_recr', 'age_recr_cont',
             'bhs', 'car_sys', 'met_sys', 'liv_fun', 'kid_fun'))) %>%
  tbl_uvregression(
    method = lm,
    y = inf_sys,
    estimate_fun = ~style_sigfig(.x, digits = 3),
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    hide_n = TRUE,
    conf.int = FALSE 
  ) %>%
  #  add_global_p(keep=TRUE) %>%  # uses car::Anova(type=III)
  bold_p(t=0.05) %>%
  italicize_levels() %>%
  bold_labels() %>%
  modify_caption("**Univariate analysis of Inflammatory system as exposure**") %>%
  modify_header(label = "**Variable**")


# Univariate - liv_fun
t5 = bb2 %>%
  select(!(c('sex','age_recr', 'age_recr_cont',
             'bhs', 'car_sys', 'inf_sys', 'met_sys', 'kid_fun'))) %>%
  tbl_uvregression(
    method = lm,
    y = liv_fun,
    estimate_fun = ~style_sigfig(.x, digits = 3),
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    hide_n = TRUE,
    conf.int = FALSE 
  ) %>%
  #  add_global_p(keep=TRUE) %>%  # uses car::Anova(type=III)
  bold_p(t=0.05) %>%
  italicize_levels() %>%
  bold_labels() %>%
  modify_caption("**Univariate analysis of Liver function as exposure**") %>%
  modify_header(label = "**Variable**")


# Univariate - kid_fun
t6 = bb2 %>%
  select(!(c('sex','age_recr', 'age_recr_cont',
             'bhs', 'car_sys', 'inf_sys', 'liv_fun', 'met_sys'))) %>%
  tbl_uvregression(
    method = lm,
    y = kid_fun,
    estimate_fun = ~style_sigfig(.x, digits = 3),
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    hide_n = TRUE,
    conf.int = FALSE 
  ) %>%
  #  add_global_p(keep=TRUE) %>%  # uses car::Anova(type=III)
  bold_p(t=0.05) %>%
  italicize_levels() %>%
  bold_labels() %>%
  modify_caption("**Univariate analysis of Kidney function as exposure**") %>%
  modify_header(label = "**Variable**")



tbl_merge(
  tbls = list(t1, t2, t3, t4, t5, t6),
  tab_spanner = c("**BHS**", "**Metabolic system**", "**Cardiovascular system**",
                  "**Inflammatory system**", "**Liver function**", "**Kidney function**"))




#   modify_header(label ~ "**Variable**") %>%
# modify_caption("**Univariate analysis of BHS as exposure** (N = {N})") %>%
  
# add_global_p() %>%  # add global p-value 
# add_q() %>%         # adjusts global p-values for multiple testing
# bold_p() %>%        # bold p-values under a given threshold (default 0.05)
# bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
# bold_labels()

