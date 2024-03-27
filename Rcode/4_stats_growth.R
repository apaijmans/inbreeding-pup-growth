# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 4_stats_growth
#
# Purpose: This script is used to run the models for pup weight gain.
#
# Date: 2024-03-20
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

## ---- growth_filter --------

# Keep only pups that survived
pup_alife <- pup_data %>% 
  filter(Survival == "1") %>%
  select(-c(Pup_Death, Survival))

##---- chunk_end



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Full model: effect of pup/mum sMLH on pup tagging weight, including mum ID as random effect  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- m1_growth --------
#~~ Weight gain incl maternal effects
m1growth <- lm(WeightGain ~ sMLH_msat39_pup
               + Pup_Sex
               + Pup_BirthWeight
               + Age_Tag
               + Year 
               + sMLH_msat39_mum
               + Mum_Age,
               #+ (1 | uniqueID_mum), # including mother ID as a random effect did not change the results significantly, and might be confounded with mum sMLH
               data = pup_alife)

summary(m1growth)

##---- chunk_end

## ---- m1_growth_assumptions --------

#~~ Model assumptions
testDispersion(m1growth)
plotQQunif(m1growth)
plotResiduals(m1growth)

##---- chunk_end

#~~ Save model
saveRDS(m1growth, file = here("Data", "Processed", "m1_growth.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Model 2: effect of pup sMLH on pup birth mass, excluding mums  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- m2_growth --------
#~~ Weight gain excl maternal effects
m2growth <- lm(WeightGain ~ sMLH_msat39_pup
               + Pup_Sex
               + Pup_BirthWeight 
               + Age_Tag
               + Year, 
               data = pup_alife)

summary(m2growth)

##---- chunk_end

## ---- m2_growth_assumptions --------

#~~ Model assumptions
testDispersion(m2growth)
plotQQunif(m2growth)
plotResiduals(m2growth)
# The residuals vs. predicted quantile plot shows a small deviation for the 0.75 quantile. However, the combined
# adjusted quantile test is not significant and also the Kolmogorov-Smirnov(KS)-test is non significant (see QQplot). 
# So I conclude that the deviation is not very big, and no reason to reject the model.

##---- chunk_end

#~~ Save model
saveRDS(m2growth, file = here("Data", "Processed", "m2_growth.rds"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Stats table for both models  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- growth_table --------

#~~ Labels
tab_labGrowth <- c(
  `(Intercept)` = "Intercept",
  sMLH_msat39_pup = "pup sMLH",
  Pup_BirthWeight = "pup birth mass",
  Pup_SexM = "pup sex [M]",
  sMLH_msat39_mum = "mother sMLH",
  Mum_Age = "mother age",
  Age_Tag = "pup age",
  Year2018 = "season [2019]",
  Year2019 = "season [2020]",
  Year2020 = "season [2021]")

#~~ Table
print(tab_model(m1growth, m2growth,
                pred.labels = tab_labGrowth,
                title = "Pup growth",
                dv.labels = c("(a) model incl. maternal effect",
                              "(b) model excl. maternal effect"),
                #order.terms = c(1, 2, 3, 9, 4, 5, 6, 7, 8),
                show.stat=T, 
                string.stat = "t value",
                file = here("Tables", "Table_growth_full_model_vs_no_mat_NEW.html")))

# Makes a screenshot of saved htlm table and saves as a png
webshot::webshot(here("Tables", "Table_growth_full_model_vs_no_mat_NEW.html"), 
                 file=here("Tables", "Table_growth_full_model_vs_no_mat_NEW.png"), delay=2, vheight = 450, vwidth = 700)

##---- chunk_end
