# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 1d_prep_maternity_files_FWB_after_checks
#
# Purpose: This script is used to generate updated mum-pup files as well as a file with males
# that can be used to re-run the NEWPAT excel macro. The difference to script 1b is that it is uses the 
# msats data after it was corrected for the genotyping mistakes that were found in the first round of the maternity analysis.
# Re-running the NEWPAT macro allows us to extract the mum-pup pairs that are a genetic match in script 1f.
# NB we do not make a new list of males, since they were not checked, since that is beyond the scope of this analysis.
#
# Date: 2023-11-27
# -----------------------------------------------------------

library(here)
library(readxl)
library(tidyverse)
library(data.table)



#~~~~~~~~~~~~~~~~~~~~~~~#
#  Load growth data  ####
#~~~~~~~~~~~~~~~~~~~~~~~#

pup_data <- openxlsx::read.xlsx(here("Data", "Processed", "pup_growth_2017-2020.xlsx"), detectDates = T) %>%
  filter(grepl("^AGPC", ID_Pup))

which(is.na(as.numeric(pup_data$Age_Tag)) != is.na(pup_data$Age_Tag))
pup_data$Age_Tag <- as.numeric(pup_data$Age_Tag)

str(pup_data)

# Check for duplicates
pup_data %>% filter(ID_Pup %in% unique(.[["ID_Pup"]][duplicated(.[["ID_Pup"]])])) # 0



#~~~~~~~~~~~~~~~~~~~~~#
#  Load msat data  ####
#~~~~~~~~~~~~~~~~~~~~~#

#~~ Code to load the genotypes dataframe
msat_genotypes <- read_excel(here("Data", "Processed", "msats_growth_individuals_after_checks.xlsx"),
                             guess_max = 16000) %>%
  select(uniqueID, dummyID, PlateNumber, Pv9.a:Mang36.b)


#~~ Which loci
loci_9_names <- c("Pv9.a", "Pv9.b",
                  "Hg6.3.a", "Hg6.3.b",
                  "Lw10.a", "Lw10.b",
                  "Hg8.10.a", "Hg8.10.b",
                  "Aa4.a", "Aa4.b",
                  "PvcA.a", "PvcA.b",
                  "PvcE.a", "PvcE.b",
                  "Hg1.3.a", "Hg1.3.b",
                  "M11a.a", "M11a.b")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Loop to generate 2 dfs for mum-pups  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Loop through following code to generate 2 files for mum-pups for the years 2018-2019

yr <- c(2018, 2019)
AGP_list <- list()
n_gaps <- 31 # maximum of 31 gaps means a minimum of 8 loci that can be compared between mom and pup
#i=1

for (i in 1:2) {
  
  #~~ Select mum-pup pairs
  AGP <- pup_data %>%
    filter(Year==yr[i]) %>%
    filter(!is.na(ID_Mum)) %>%
    select(ID_Pup, ID_Mum) %>%
    #unite(dummy_PUP, ID_Pup, uniqueID_pup, sep = "/") %>%
    #unite(dummy_MUM, ID_Mum, uniqueID_mum, sep = "/") %>%
    arrange(ID_Pup)
  
  AGP$pair <- 1:nrow(AGP)
  
  AGP_gen <- AGP %>% pivot_longer(c( ID_Mum, ID_Pup)) %>%
    #separate(value, into = c("id", "uniqueID"), sep = "/") %>%
    left_join(msat_genotypes, by = c("value" = "dummyID")) %>%
    mutate(name=ifelse(name=="ID_Mum", "F", "O")) %>%
    select(pair, SampleID=value, lifeStage=name, Pv9.a:Mang36.b, PlateNumber) 
  
  
  #~~ Remove all pairs where one or both individuals have more than x gaps
  
  # Make column with number of gaps/individual
  #AGP_gen$gaps9 = apply(AGP_gen %>% select(all_of(loci_9_names)), 1, function(x) sum(is.na(x))/2)
  #AGP_gen$gaps39 = apply(AGP_gen %>% select(Pv9.a:Mang36.b), 1, function(x) sum(is.na(x))/2)
  
  compare_genotypes9 <- AGP_gen %>%
    group_by(pair) %>%
    summarise(across(all_of(loci_9_names), ~if(any(is.na(.))) NA else as.integer(n_distinct(.) == 1)))
  
  compare_genotypes9$missing_loci = rowSums(is.na(compare_genotypes9))/2
  
  pairs_to_remove9 <- pull(compare_genotypes9 %>% 
                             filter(missing_loci >1) %>%
                             select(pair))
  
  compare_genotypes39 <- AGP_gen %>%
    group_by(pair) %>%
    summarise(across(Pv9.a:Mang36.b, ~if(any(is.na(.))) NA else as.integer(n_distinct(.) == 1)))
  
  compare_genotypes39$missing_loci = rowSums(is.na(compare_genotypes39))/2
  
  pairs_to_remove39 <- pull(compare_genotypes39 %>% 
                              filter(missing_loci > n_gaps) %>%
                              select(pair))
  
  #~~ Print how many pairs will be removed
  print(paste0("n mum-pup pairs removed from ", yr[i], ": ", length(pairs_to_remove39)))
  
  AGP_gen <- AGP_gen %>%
    filter(!pair %in% pairs_to_remove39)
  
  #~~ Check whether number of O equals number of F
  print(AGP_gen %>%
          group_by(lifeStage) %>%
          tally() %>%
          ungroup())
  
  # AGP_gen <- AGP_gen %>% select(-gaps9)
  
  AGP_list[[i]] <- AGP_gen
  
}

# [1] "n mum-pup pairs removed from 2018: 0"
# # A tibble: 2 x 2
# lifeStage     n
# 1 F            26
# 2 O            26
# [1] "n mum-pup pairs removed from 2019: 0"
# # A tibble: 2 x 2
# lifeStage     n
# 1 F            25
# 2 O            25



#~~~~~~~~~~~~~~~#
#  Save dfs  ####
#~~~~~~~~~~~~~~~#

# Save mum-pup pairs 2 years together
all_AGP <- bind_rows(AGP_list)

openxlsx::write.xlsx(all_AGP, 
                     here("Data", "Processed", "Maternity", "2018-19_m-p_mat_FWB_after_checks.xlsx"))

