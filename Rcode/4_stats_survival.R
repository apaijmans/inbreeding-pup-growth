# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 4_stats_survival
#
# Purpose: This script is used to run the models for pup birth survival.
#
# Date: 2024-01-11
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest) 
library(DHARMa)
library(sjPlot)
library(cowplot)



#~~~~~~~~~~~~~~~~#
#  Load data  ####
#~~~~~~~~~~~~~~~~#

pup_data <- openxlsx::read.xlsx(here("Data", "Processed", "filtered_pup_survival.xlsx"), detectDates = T) %>%
  mutate(Pup_Sex = as.factor(Pup_Sex)) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Survival = as.factor(Survival)) 

#head(pup_growth)

str(pup_data)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Full model: effect of pup/mum sMLH on pup survival  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- m1_survival --------
#~~ Survival incl maternal effects
m1survival <- glm(Survival ~ sMLH_msat39_pup
                  + Pup_Sex 
                  + Pup_BirthWeight
                  + Year
                  + sMLH_msat39_mum
                  + Mum_Age,
                  #+ (1 | uniqueID_mum), # including mother ID as a random effect did not change the results significantly, and might be confounded with mum sMLH
                  family=binomial, 
                  data = pup_data)
# # Because of convergence issues for the model incl mother ID as a random effect, I had to re-run it with more iterations
# 
# ss <- getME(m1survival, c("theta", "fixef"))
# m1survival.m2 <- update(m1survival, start = ss, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

summary(m1survival) # conclusions are the same as for the model excl mum ID as a random factor

##---- chunk_end

## ---- m1_survival_assumptions --------

#~~ Model assumptions
testDispersion(m1survival)
plotQQunif(m1survival)
plotResiduals(m1survival)

##---- chunk_end

#~~ Save model
saveRDS(m1survival, file = here("Data", "Processed", "m1_survival.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Model 2: effect of pup sMLH on pup survival, excluding mums  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- m2_survival --------
#~~ Survival excl maternal effects
m2survival <- glm(Survival ~ sMLH_msat39_pup
                  + Pup_Sex 
                  + Pup_BirthWeight
                  + Year,
                  family=binomial, 
                  data = pup_data) 

summary(m2survival)

##---- chunk_end

## ---- m2_survival_assumptions --------

#~~ Model assumptions
testDispersion(m2survival)
plotQQunif(m2survival)
plotResiduals(m2survival)

##---- chunk_end

#~~ Save model
saveRDS(m2survival, file = here("Data", "Processed", "m2_survival.rds"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Stats table for both models  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- survival_table --------

#~~ Labels
tab_labSurvival <- c(
  `(Intercept)` = "Intercept",
  sMLH_msat39_pup = "pup sMLH",
  Pup_BirthWeight = "pup birth mass",
  Pup_SexM = "pup sex [M]",
  sMLH_msat39_mum = "mother sMLH",
  Mum_Age = "mother age",
  Year2018 = "season [2019]",
  Year2019 = "season [2020]",
  Year2020 = "season [2021]")

#~~ Table
print(tab_model(m1survival, m2survival,
                pred.labels = tab_labSurvival,
                title = "Pup survival",
                dv.labels = c("(a) model incl. maternal effect",
                              "(b) model excl. maternal effect"),
                #order.terms = c(1, 2, 4, 3, 7, 5, 6),
                transform = NULL,
                show.stat=T, 
                string.stat = "t value",
                file = here("Tables", "Table_survival_full_model_vs_no_mat.html")))

#~~ Makes a screenshot of saved htlm table and saves as a png
webshot::webshot(here("Tables", "Table_survival_full_model_vs_no_mat.html"), 
                 file=here("Tables", "Table_survival_full_model_vs_no_mat.png"), delay=2, vheight = 400, vwidth = 700)

##---- chunk_end
