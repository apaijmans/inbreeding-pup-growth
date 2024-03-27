# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 5_figs_stats
#
# Purpose: This script is used to make figures using 
# the statistical model outputs
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



#~~~~~~~~~~~~~~~~~~#
#  Load models  ####
#~~~~~~~~~~~~~~~~~~#

m1birthmass <- readRDS(here("Data", "Processed", "m1_birthmass.rds"))
m1survival <- readRDS(here("Data", "Processed", "m1_survival.rds"))
m1growth <- readRDS(here("Data", "Processed", "m1_growth.rds"))

m2birthmass <- readRDS(here("Data", "Processed", "m2_birthmass.rds"))
m2survival <- readRDS(here("Data", "Processed", "m2_survival.rds"))
m2growth <- readRDS(here("Data", "Processed", "m2_growth.rds"))

## ---- fig_msat_models --------

#~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Make general theme  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Use Martin Stoffel's GGplot theme as a base
source(here("Rcode", "anneke_theme.R"))

#~~ Make a list for the theme so it is the same for all figures
gglayer_theme <- list(
  geom_point(shape = 22, size = 3, fill = "black"),
  theme_anneke(),
  theme(axis.line.x = element_line(colour = 'black', linetype='solid'),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.y = element_text(colour = 'black'),
        plot.title = element_text(size = rel(1)))
)

plot_label <- c(
  `(Intercept)` = "Intercept",
  sMLH_msat39_pup = "pup sMLH",
  Pup_SexM = "pup sex [M]",
  Pup_BirthWeight = "pup birth mass",
  sMLH_msat39_mum = "mother sMLH",
  Mum_Age = "mother age",
  Age_Tag = "pup age",
  Year2018 = "season [2019]",
  Year2019 = "season [2020]",
  Year2020 = "season [2021]")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Apply custom plot function to 3 models  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source(here("Rcode", "custom_forest_plot.R"))

#~~ Create labels
lab1 <- paste0("(a) Pup birth mass\nIncl. maternal effects\n n = ", nobs(m1birthmass))
lab2 <- paste0("(b) Pup survival\nIncl. maternal effects\n n = ", nobs(m1survival))
lab3 <- paste0("(c) Pup growth\nIncl. maternal effects\n n = ", nobs(m1growth))

lab4 <- paste0("(d) Pup birth mass\nExcl. maternal effects\n n = ", nobs(m2birthmass))
lab5 <- paste0("(e) Pup survival\nExcl. maternal effects\n n = ", nobs(m2survival))
lab6 <- paste0("(f) Pup growth\nExcl. maternal effects\n n = ", nobs(m2growth))

#~~ Make plots
p.bw <- plot_data_models(m1birthmass, lab1, gglayer_theme)
p.surv <- plot_data_models(m1survival, lab2, gglayer_theme)
p.wg <- plot_data_models(m1growth, lab3, gglayer_theme)

p2.bw <- plot_data_models(m2birthmass, lab4, gglayer_theme)
p2.surv <- plot_data_models(m2survival, lab5, gglayer_theme)
p2.wg <- plot_data_models(m2growth, lab6, gglayer_theme)
# nb warnings are because I am removing dots and adding squares in the function!

#plot(p.bw)

#~~ Save plots
all_plots <- cowplot::plot_grid(p.bw, p.surv, p.wg,
                                p2.bw, p2.surv, p2.wg,
                                nrow = 2)

all_plots

##---- chunk_end

cowplot::save_plot(here("Figs", "F2_Forest_plots_incl_excl_mat.jpg"), all_plots, base_width = 8, base_height = 7, dpi = 300)
