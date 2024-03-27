# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 2_calculate_sMLH
#
# Purpose: This script calculates standardised multi-locus 
# heterozygosity (sMLH) and identity disequilibrium (g2) for 
# both microsatellite data and SNP data
#
# Date: 2023-12-01
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)
library(inbreedR)
library(ggtext)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Load growth data (incl FWB individuals)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- pup_data --------

#~~ Load pup data
pup_data <- openxlsx::read.xlsx(here("Data", "Processed", "pup_growth_mums_checked.xlsx"), detectDates = T) %>%
  mutate(Pup_Sex = as.factor(Pup_Sex)) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Age_Tag = as.integer(Age_Tag))

##---- chunk_end

str(pup_data)

#~~ Check for duplicates
pup_data %>% filter(ID_Pup %in% unique(.[["ID_Pup"]][duplicated(.[["ID_Pup"]])])) # 0



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Import msats data and exclude genotypes with missing data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- msat_data --------

#~~ Load msats for all individuals in this study
msats_all <- openxlsx::read.xlsx(here("Data", "Processed", "msats_growth_individuals_after_checks.xlsx"), detectDates = T) %>%
  mutate(Gaps = as.numeric(Gaps))

##---- chunk_end

#~~ Check for duplicates
msats_all %>% filter(uniqueID %in% unique(.[["uniqueID"]][duplicated(.[["uniqueID"]])])) %>% 
  select(uniqueID, SampleID) # zero


#~~ Explore missing data

# Create column for gaps based on the original 9 loci
loci_9_names <- c("Pv9.a", "Pv9.b",
                  "Hg6.3.a", "Hg6.3.b",
                  "Hg8.10.a", "Hg8.10.b",
                  "Hg1.3.a", "Hg1.3.b",
                  "M11a.a", "M11a.b",
                  "PvcA.a", "PvcA.b",
                  "Lw10.a", "Lw10.b",
                  "Aa4.a", "Aa4.b",
                  "PvcE.a", "PvcE.b")

# Dummy df to explore
msats_all2 <-  msats_all

# Calculate # gaps based on 9 original loci
msats_all2$gaps9 = apply(msats_all[,loci_9_names] %>% select(all_of(loci_9_names)), 1, function(x) sum(is.na(x))/2)

# All individuals with more than 2 gaps (if 9 loci genotyped) or more than 4 gaps (if 39 loci genotyped)
nrow(msats_all2 %>%
       filter(is.na(Gaps) & gaps9 > 2 | Gaps > 4)) # we lose 87 individuals, incl FWB

nrow(msats_all2 %>%
       filter(is.na(Gaps) & gaps9 > 2)) # 10 individuals genotyped for 9 and with more than 2 missing

# All individuals that were only genotyped for 9 loci or had more than 4 gaps when genotyped for 39 loci
nrow(msats_all2 %>%
       filter(is.na(Gaps)| Gaps > 4)) # we lose 88 individuals, incl FWB

# Which individuals were genotyped for 39 loci and had more than 4 missing loci
msats_all2 %>%
  filter(Gaps > 4) %>%
  select(uniqueID, SampleID, PlateNumber, Gaps, gaps9) # 77 incl FWB
# Mostly pups with failed extractions and 9 mums

# view(msats_all3 %>% select(SampleID, PlateNumber, Gaps, gaps9))
# view(msats_all2 %>% select(SampleID, PlateNumber, Gaps, gaps9))         

## ---- msat_no_gaps --------

#~~ Keep only individuals genotyped for all 39 loci, with a maximum of 4 missing loci
msats_all <- msats_all %>%
  filter(Gaps < 5)

##---- chunk_end



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Calculate sMLH for msats  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- sMLH_msats --------

#~~ Convert msat data to inbreedR input
msat_genotypes39 <-  msats_all %>% select(Pv9.a:Mang36.b)
msat_ids <- msats_all %>% select(uniqueID)

msat_genotypes39_raw <- convert_raw(msat_genotypes39)

#~~ Check if data is in right format for InbreedR
check_data(msat_genotypes39_raw, num_ind = nrow(msat_ids), num_loci = length(msat_genotypes39_raw))

#~~ Calculate sMLH (incl FWB)
het39 <- sMLH(msat_genotypes39_raw)

sMLH_msat39 <- cbind(msat_ids, het39)
colnames(sMLH_msat39) <- c("uniqueID", "sMLH_msat39")

#~~ Add sMLH to pup data
pup_data <- left_join(pup_data, sMLH_msat39, by = c("uniqueID_pup" = "uniqueID")) %>%
  rename(sMLH_msat39_pup=sMLH_msat39)


pup_data <- left_join(pup_data, sMLH_msat39, by = c("uniqueID_mum" = "uniqueID"))%>%
  rename(sMLH_msat39_mum=sMLH_msat39)

##---- chunk_end

#~~ Some checking
nrow(pup_data %>% filter(!grepl("^AGPC", ID_Pup)) %>% filter(!is.na(sMLH_msat39_pup))) # 1122 pups with sMLH info
nrow(pup_data %>% filter(!is.na(sMLH_msat39_pup))) # 1171 incl FWB
nrow(pup_data %>% filter(!grepl("^AGPC", ID_Pup)) %>% filter(!is.na(sMLH_msat39_mum))) # 549 mums with sMLH info
nrow(pup_data %>% filter(!is.na(sMLH_msat39_mum))) # 598 incl FWB

#view(pup_data %>% filter(is.na(sMLH_msat39_pup) & !is.na(sMLH_msat39_mum)))

# How many pairs where both pup and mum have sMLH info?
nrow(pup_data %>% filter(!is.na(sMLH_msat39_pup) & !is.na(sMLH_msat39_mum))) # 563 pups with sMLH and mums with sMLH info



#~~~~~~~~~~~~~~~~#
#  Save data  ####
#~~~~~~~~~~~~~~~~#

openxlsx::write.xlsx(pup_data,
                     here("Data", "Processed", "growth_sMLH_msats.xlsx"), quote = F,
                     row.names = F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Calculate sMLH for SNPs  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- sMLH_SNPs --------

# To save time, run code below only once, then save the output

# #~~ Load SNP data
# snp_data <- data.table::fread(here("Data", "Processed", "SNPs", "NEW_POS_Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes.raw"))
# 
# #str(snp_data)
# 
# 
# #~~ Convert data to inbreedR input
# snp_genotypes <-  snp_data[,-c(1:6)]
# snp_ids <- snp_data %>% select(2)
# 
# # .raw is encoded such that the SNP has _x with x being the minor allele. 0 means 0 counts of the minor allele, 1 means 1 count and 2 means two.
# # eg SNP_G 0 = CC, 1 = GC or CG, 2 = GG
# # So, 1 is het, 0 or 2 is hom
# 
# #~~ convert all 2s into 0s, so that all homozygotes are 0 and heterozygotes are 1
# snp_genotypes[snp_genotypes == 2] <- 0
# 
# #~~ Check if data is in right format for InbreedR
# check_data(snp_genotypes, num_ind = nrow(snp_ids), num_loci = length(snp_genotypes))
# 
# #~~ Calculate sMLH
# hetSNPs <- sMLH(snp_genotypes)
# 
# sMLH_SNPs <- cbind(snp_ids, hetSNPs)
# colnames(sMLH_SNPs) <- c("ID", "sMLH_SNPs")
# 
# #~~ save SNP sMLH data
# write.table(sMLH_SNPs,
#             here("Data", "Processed", "sMLH_SNPs.txt"),
#             quote = F,
#             row.names = F)

#~~ Load data that was generated in the script above
sMLH_SNPs <- read.table(here("Data", "Processed", "sMLH_SNPs.txt"), 
                        stringsAsFactors = F, 
                        header = T)

##---- chunk_end


# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# #  sMLH - Froh correlation plot  ####
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 
# # How does the sMLH calculated using the SNP data correlate with Froh calculated using the same data?
# DataRM_Day60 <- read.csv(here("Data", "Raw", "GrowthRM_BI1820_Day60.new.csv")) 
# 
# sMLH_Froh <- bind_rows(
#   DataRM_Day60 %>% select(ID, froh) %>% distinct(., .keep_all = T) %>% mutate(animal = "pup"),
#   DataRM_Day60 %>% select(ID = Mum_ID, froh = froh_mum) %>% distinct(., .keep_all = T) %>% mutate(animal = "mum")) %>%
#   full_join(sMLH_SNPs)
# 
# #~~ Correlation plot
# P.sMLH.Froh <- ggpubr::ggscatter(sMLH_Froh,
#                             x = "froh",
#                             xlab = "Froh",
#                             y = "sMLH_SNPs",
#                             ylab = "sMLH",
#                             add = "reg.line",
#                             conf.int = TRUE,
#                             cor.coef = TRUE,
#                             cor.method = "pearson",
#                             na.rm = TRUE)
# P.sMLH.Froh
# # Sig negative correlation (makes sense: a large Froh means that a relatively large part 
# # of the genome is in ROH, so relatively low heterozygosity, ergo, low sMLH)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  sMLH correlation plot  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# # How does the sMLH based on msats correlate with the sMLH based on SNPs?
# 
# #~~ Rebecca's "conversion" file to merge in BAS ids
# IDpups <- read.csv(here("C:/Uni/06-SNPchip/Data/raw/attendance/Mum-Pup_BASid.csv"), header = T) %>%
#   rename(Rebecca_ID = ID)
# 
# head(IDpups)
# 
# #Fix typo
# IDpups[which(IDpups$BAS_ID == "AGPF19003"), "BAS_ID"] <- "AGFC19003"
# IDpups[which(IDpups$BAS_ID == "AFP19064"), "BAS_ID"] <- "AGP19064"
# 
# 
# sMLH_SNPs <- full_join(sMLH_SNPs, IDpups, by = c("ID" = "Rebecca_ID")) %>%
#   select(BAS_ID, sMLH_SNPs)
# 
# head(sMLH_SNPs)
# 
# 
# #~~ Wrangle pup_data file
# exp_pups <- pup_data %>%
#   filter(Study == "experimental_animals") %>%
#   select(BAS_ID = ID_Pup, uniqueID = uniqueID_pup, sMLH_msat39 = sMLH_msat39_pup)
# 
# exp_mums <- pup_data %>%
#   filter(Study == "experimental_animals") %>%
#   select(BAS_ID = ID_Mum, uniqueID = uniqueID_mum, sMLH_msat39 = sMLH_msat39_mum)
# 
# sMLH_msat_exp <- rbind(exp_pups, exp_mums)
# 
# sMLH_both <- full_join(sMLH_SNPs, sMLH_msat_exp, by = "BAS_ID")
# 
# head(sMLH_both)
# 
# #~~ Correlation plot
# P.sMLH <- ggpubr::ggscatter(sMLH_both,
#                             x = "sMLH_SNPs",
#                             xlab = "sMLH 75k autosomal SNPs",
#                             y = "sMLH_msat39",
#                             ylab = "sMLH 39 microsatellites",
#                             add = "reg.line",
#                             conf.int = TRUE,
#                             cor.coef = TRUE,
#                             cor.method = "pearson",
#                             na.rm = TRUE)
# P.sMLH
# # No sig correlation
# 
# # ggsave(here("Figs", "sMLH_correlation.jpg"), P.sMLH, width = 5, height = 5)
# 
# #~~ Split by beach
# P.FWB <- ggpubr::ggscatter(sMLH_both %>% filter(grepl("^AGPC", BAS_ID) | grepl("^AGFC", BAS_ID)),
#                            x = "sMLH_SNPs",
#                            xlab = "sMLH 75k autosomal SNPs",
#                            y = "sMLH_msat39",
#                            ylab = "sMLH 39 microsatellites",
#                            title = "FWB mums and pups",
#                            add = "reg.line",
#                            conf.int = TRUE,
#                            cor.coef = TRUE,
#                            cor.method = "pearson",
#                            na.rm = TRUE)
# P.FWB
# 
# P.SSB <- ggpubr::ggscatter(sMLH_both %>% filter(grepl("^AGP1", BAS_ID) | grepl("^AGF1", BAS_ID)),
#                            x = "sMLH_SNPs",
#                            xlab = "sMLH 75k autosomal SNPs",
#                            y = "sMLH_msat39",
#                            ylab = "sMLH 39 microsatellites",
#                            title = "SSB mums and pups",
#                            add = "reg.line",
#                            conf.int = TRUE,
#                            cor.coef = TRUE,
#                            cor.method = "pearson",
#                            na.rm = TRUE)
# P.SSB
# # Sig positive correlation for SSB samples
# 
# p.both <- cowplot::plot_grid(P.SSB, P.FWB)
# p.both
# # Directions seem opposite for the beaches, and in fact significantly positive for SSB (p = 0.022)
# 
# # cowplot::save_plot(here("Figs", "sMLH_correlation_plots.jpg"), p.both, base_width = 7, base_height = 5)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Calculate g2 (variation in inbreeding) msats  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- g2_msats_SSB --------

#~~ Calculate g2 for all SSB samples (ie excl FWB)
# msat_genotypes39 <-  msats_all %>% filter(!grepl("^AGPC", dummyID) & !grepl("^AGFC", dummyID)) %>% select(Pv9.a:Mang36.b)
# msat_ids <- msats_all %>% filter(!grepl("^AGPC", dummyID) & !grepl("^AGFC", dummyID)) %>% select(uniqueID)
# 
# msat_genotypes39_raw <- convert_raw(msat_genotypes39)
# 
# g2_39loci_SSB <- g2_microsats(msat_genotypes39_raw, nperm = 1000, nboot = 1000, CI = 0.95)
# 
# save(g2_39loci_SSB, file = "Data/Processed/g2_39loci_after_checks_SSB.RData")

load(here("Data", "Processed", "g2_39loci_after_checks_SSB.RData"))

# Summary
g2_39loci_SSB

##---- chunk_end

# Calculation of identity disequilibrium with g2 for microsatellite data
# ----------------------------------------------------------------------
#   
# Data: 1392 observations at 39 markers
# Function call = g2_microsats(genotypes = msat_genotypes39_raw, nperm = 1000,     nboot = 1000, CI = 0.95)
# 
# g2 = 0.0006590586, se = 0.0003574331
# 
# confidence interval 
# 2.5%         97.5% 
#   -4.535789e-05  1.303660e-03 
# 
# p (g2 > 0) = 0.021 (based on 1000 permutations)



#~~ Calculate g2 for all samples
# 
# msat_genotypes39 <-  msats_all %>% select(Pv9.a:Mang36.b)
# msat_ids <- msats_all %>% select(uniqueID)
# 
# msat_genotypes39_raw <- convert_raw(msat_genotypes39)
# 
# g2_39loci <- g2_microsats(msat_genotypes39_raw, nperm = 1000, nboot = 1000, CI = 0.95)
# 
# save(g2_39loci, file = "Data/Processed/g2_39loci_after_checks.RData")

## ---- g2_msats --------

load(here("Data", "Processed", "g2_39loci_after_checks.RData"))

# summary
g2_39loci

##---- chunk_end

# Calculation of identity disequilibrium with g2 for microsatellite data
# ----------------------------------------------------------------------
#   
#   Data: 1490 observations at 39 markers
# Function call = g2_microsats(genotypes = msat_genotypes39_raw, nperm = 1000,     nboot = 1000, CI = 0.95)
# 
# g2 = 0.0005540813, se = 0.0003238919
# 
# confidence interval 
# 2.5%         97.5% 
#   -8.075412e-05  1.224770e-03 
# 
# p (g2 > 0) = 0.028 (based on 1000 permutations)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Calculate g2 (variation in inbreeding) SNPs  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- g2_SNPs_SSB --------

# SNP g2 values (file generated in "calculate_g2.R" script that was ran on the server)
load(here("Data", "Processed", "g2_snps_SSB.RData"))

g2_snp_geno_SSB

##---- chunk_end

# Calculation of identity disequilibrium with g2 for SNP data
# -----------------------------------------------------------
#   
#   Data: 98 observations at 75101 markers
# Function call = g2_snps(genotypes = snp_genotypes_SSB, nperm = 1000, nboot = 1000,     CI = 0.95, parallel = T, ncores = 8)
# 
# g2 = 0.0001625322, se = 2.841268e-05
# 
# confidence interval 
# 2.5%        97.5% 
#   0.0001073699 0.0002159702 
# 
# p (g2 > 0) = 0.001 (based on 999 permutations)



## ---- g2_SNPs --------

# SNP g2 values (file generated in "calculate_g2.R" script that was ran on the server)
load(here("Data", "Processed", "g2_snps.RData"))

# summary SNPs
g2_snp_geno

##---- chunk_end

# Calculation of identity disequilibrium with g2 for SNP data
# -----------------------------------------------------------
#   
#   Data: 196 observations at 75101 markers
# Function call = g2_snps(genotypes = snp_genotypes, nperm = 1000, nboot = 1000,     CI = 0.95, parallel = T, ncores = 8)
# 
# g2 = 0.0001179302, se = 1.753435e-05
# 
# confidence interval 
# 2.5%        97.5% 
#   8.597162e-05 1.523841e-04 
# 
# p (g2 > 0) = 0.001 (based on 999 permutations)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  g2 figure SNPs and msats - all  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- g2_fig --------

# Make df for plotting
g2_plot <- rbind(data.frame(g2_boot = g2_snp_geno$g2_boot, gen_data = "snps"),
                 data.frame(g2_boot = g2_39loci$g2_boot, gen_data = "msats"))

lcl_snps <- g2_snp_geno$CI_boot[1]
ucl_snps <- g2_snp_geno$CI_boot[2]
g2_boot_summary_snps <- data.frame(lcl_snps, ucl_snps)

lcl_msat <- g2_39loci$CI_boot[1]
ucl_msat <- g2_39loci$CI_boot[2]
g2_boot_summary_msat <- data.frame(lcl_msat, ucl_msat)

n_snps <- g2_snp_geno$nobs
n_msat <- g2_39loci$nobs

# Colors
col1 <- "#872ca2"
col2 <- "#f6a97a"

# Use Martin Stoffel's GGplot theme as a base
source(here("Rcode", "anneke_theme.R"))

ggplot(g2_plot, aes(x = g2_boot, fill = gen_data)) + 
  geom_histogram(alpha = 0.7, position = "identity", binwidth = 0.00001) + # 0.00001 or 0.00005 ?
  scale_fill_manual(values = c(col1, col2), labels = c(paste0("msats\n(n = ", n_msat, ")"), paste0("SNPs\n(n = ", n_snps, ")"))) +
  # Add CI bars and g2 line for msats
  geom_errorbarh(aes(xmin = g2_boot_summary_msat$lcl_msat , xmax = g2_boot_summary_msat$ucl_msat , y = 250),
                 linewidth = 0.8, color = col1, linetype = "solid", height = 0) +
  geom_vline(xintercept = g2_39loci$g2, linewidth = 0.6, color = col1, linetype = "dashed") +
  # Add CI bars and g2 line for SNPs  
  geom_errorbarh(aes(xmin = g2_boot_summary_snps$lcl_snps , xmax = g2_boot_summary_snps$ucl_snps , y = 255),
                 linewidth = 0.8, color = col2, linetype = "solid", height = 0) +
  geom_vline(xintercept = g2_snp_geno$g2, linewidth = 0.6, color = col2, linetype = "dashed") +  
  # Add zero line
  geom_vline(xintercept = 0, linewidth = 0.6, linetype = "solid") +
  # Add other labs and theme
  labs(y = "Counts", x = expression(italic(g[2]))) +
  theme_anneke() +
  theme(legend.title=element_blank())

##---- chunk_end

ggsave(here("Figs", "g2_plot_msats_snps.jpg"), width = 7, height = 5)

# # # Add p-values into the plot
# # Add p values
# annotate("richtext", x=g2_boot_summary_msat$ucl_msat, y=200, label = p_msats, fill = NA, label.color = NA, hjust = -0.05, size = 4) +
# annotate("richtext", x=g2_boot_summary_snps$ucl_snps, y=206, label=p_snps, fill = NA, label.color = NA, hjust = -0.05, size = 4) +
# # P values
# p_msats <- paste0("p (*g<sub>2</sub>* > 0) = ", g2_39loci$p_val)
# p_snps <- paste0("p (*g<sub>2</sub>* > 0) = ", g2_snp_geno$p_val)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  g2 figure SNPs and msats - SSB  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## ---- g2_fig_SSB --------

# Make df for plotting
g2_plot <- rbind(data.frame(g2_boot = g2_snp_geno_SSB$g2_boot, gen_data = "snps_SSB"),
                 data.frame(g2_boot = g2_39loci_SSB$g2_boot, gen_data = "msats_SSB"))

head(g2_plot)

lcl_snps <- g2_snp_geno_SSB$CI_boot[1]
ucl_snps <- g2_snp_geno_SSB$CI_boot[2]
g2_boot_summary_snps_SSB <- data.frame(lcl_snps, ucl_snps)

lcl_msat <- g2_39loci_SSB$CI_boot[1]
ucl_msat <- g2_39loci_SSB$CI_boot[2]
g2_boot_summary_msat_SSB <- data.frame(lcl_msat, ucl_msat)

n_snps <- g2_snp_geno_SSB$nobs
n_msat <- g2_39loci_SSB$nobs

# Colors
col1 <- "#872ca2"
col2 <- "#f6a97a"

# Use Martin Stoffel's GGplot theme as a base
source("Rcode/anneke_theme.R")

ggplot(g2_plot, aes(x = g2_boot, fill = gen_data)) + 
  geom_histogram(alpha = 0.7, position = "identity", binwidth = 0.00001) + # 0.00001 or 0.00005 ?
  scale_fill_manual(values = c(col1, col2), labels = c(paste0("msats\n(n = ", n_msat, ")"), paste0("SNPs\n(n = ", n_snps, ")"))) +
  # Add CI bars and g2 line for msats
  geom_errorbarh(aes(xmin = g2_boot_summary_msat_SSB$lcl_msat , xmax = g2_boot_summary_msat_SSB$ucl_msat , y = 140),
                 linewidth = 0.8, color = col1, linetype = "solid", height = 0) +
  geom_vline(xintercept = g2_39loci_SSB$g2, linewidth = 0.6, color = col1, linetype = "dashed") +
  # Add CI bars and g2 line for SNPs  
  geom_errorbarh(aes(xmin = g2_boot_summary_snps_SSB$lcl_snps , xmax = g2_boot_summary_snps_SSB$ucl_snps , y = 145),
                 linewidth = 0.8, color = col2, linetype = "solid", height = 0) +
  geom_vline(xintercept = g2_snp_geno_SSB$g2, linewidth = 0.6, color = col2, linetype = "dashed") +  
  # Add zero line
  geom_vline(xintercept = 0, linewidth = 0.6, linetype = "solid") +
  labs(y = "Counts", x = expression(italic(g[2]))) +
  theme_anneke() +
  theme(legend.title=element_blank())

##---- chunk_end

ggsave(here("Figs", "g2_plot_msats_snps_SSB.jpg"), width = 7, height = 5)
