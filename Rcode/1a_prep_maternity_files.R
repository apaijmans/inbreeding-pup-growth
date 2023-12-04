# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 1a_prep_maternity_files
#
# Purpose: This script is used to generate mum-pup files as well as a file with males
# that can be used to run the NEWPAT excel macro on for maternity checking
# Any scoring mistakes/genotyping errors that were found were manually corrected in the file:
# Data -> Processed -> msats_growth_individuals_after_checks.xlsx
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
  filter(!grepl("^AGPC", ID_Pup))

which(is.na(as.numeric(pup_data$Age_Tag)) != is.na(pup_data$Age_Tag))
pup_data$Age_Tag <- as.numeric(pup_data$Age_Tag)

str(pup_data)

# Check for duplicates
pup_data %>% filter(ID_Pup %in% unique(.[["ID_Pup"]][duplicated(.[["ID_Pup"]])])) # 0



#~~~~~~~~~~~~~~~~~~~~~#
#  Load msat data  ####
#~~~~~~~~~~~~~~~~~~~~~#

#~~ Code to load the genotypes dataframe
msat_genotypes <- read_excel(here("Data", "Processed", "msats_growth_individuals.xlsx"),
                             guess_max = 16000) %>%
  select(uniqueID, dummyID, PlateNumber, Pv9.a:Mang36.b)


#~~ Which loci?
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
#  Loop to generate 3 dfs for mum-pups  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Loop through following code to generate 3 files for mum-pups for the years 2017-2019

yr <- c(2017, 2018, 2019, 2020)
AGP_list <- list()
n_gaps <- 31 # maximum of 31 gaps means a minimum of 8 loci that can be compared between mom and pup
#i=1

for (i in 1:4) {
  
  #~~ Select mum-pup pairs
  AGP <- pup_data %>%
    filter(Year==yr[i]) %>%
    filter(!is.na(ID_Mum)) %>%
    select(ID_Pup, uniqueID_pup, ID_Mum, uniqueID_mum) %>%
    unite(dummy_PUP, ID_Pup, uniqueID_pup, sep = "/") %>%
    unite(dummy_MUM, ID_Mum, uniqueID_mum, sep = "/") %>%
    arrange(dummy_PUP)
  
  AGP$pair <- 1:nrow(AGP)
  
  AGP_gen <- AGP %>% pivot_longer(c(dummy_MUM, dummy_PUP)) %>%
    separate(value, into = c("id", "uniqueID"), sep = "/") %>%
    left_join(msat_genotypes, by = "uniqueID") %>%
    mutate(name=ifelse(name=="dummy_MUM", "F", "O")) %>%
    select(pair, SampleID=dummyID, lifeStage=name, Pv9.a:Mang36.b, PlateNumber) 
  
  
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

# [1] "n mum-pup pairs removed from 2017: 0"
# # A tibble: 2 x 2
# lifeStage     n
# <chr>     <int>
#   1 F           155
#   2 O           155
# [1] "n mum-pup pairs removed from 2018: 3"
# # A tibble: 2 x 2
# lifeStage     n
# <chr>     <int>
#   1 F           104
#   2 O           104
# [1] "n mum-pup pairs removed from 2019: 5"
# # A tibble: 2 x 2
# lifeStage     n
# <chr>     <int>
#   1 F           157
#   2 O           157
# [1] "n mum-pup pairs removed from 2020: 2"
# # A tibble: 2 x 2
# lifeStage     n
# <chr>     <int>
#   1 F           140
#   2 O           140



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Loop to generate df for males  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Loop through following code to generate list of males from years 2015-2019

all_genotypes <- read_excel(here("Data", "Raw", "all_msat_genotypes_uniqueID.xlsx"),
                            guess_max = 16000)

#~~ Keep 1 sample for each group of unique IDs, picking the one with Best_genotype = 1 or NA
AFS <- all_genotypes %>%
  group_by(uniqueID) %>%
  filter(Best_genotype == max(Best_genotype)|is.na(Best_genotype)) %>%
  ungroup()

year_m <- c(2015, 2016, 2017, 2018, 2019, 2020)
AGM_gen <- NULL

for (i in 1:6) {
  
  #~~ Keep only males with specific birth year
  AGM <- AFS %>%
    filter(grepl(paste0("^AGM", substr(year_m[i], 3, 4),".*"), dummyID)) %>%
    mutate(lifeStage="M")%>%
    select(SampleID=dummyID, lifeStage, Pv9.a:Mang36.b)
  
  #~~ Remove all pairs where one or both individuals have more than x gaps
  # Make column with number of gaps/individual
  AGM$gaps39 = apply(AGM %>% select(Pv9.a:Mang36.b), 1, function(x) sum(is.na(x))/2)
  
  #~~ Print how many groups will be removed
  print(paste0("n males removed from ", year_m[i], ": ", nrow(AGM %>% filter(gaps39 > n_gaps))))
  
  AGM <- AGM %>%
    filter(!gaps39 > n_gaps)
  
  AGM <- AGM %>% select(-gaps39)
  
  AGM_gen <- rbind(AGM_gen, AGM) 
  
}

# [1] "n males removed from 2015: 0"
# [1] "n males removed from 2016: 0"
# [1] "n males removed from 2017: 0"
# [1] "n males removed from 2018: 0"
# [1] "n males removed from 2019: 2"
# [1] "n males removed from 2020: 4"

#rm(AGM)



#~~~~~~~~~~~~~~~#
#  Save dfs  ####
#~~~~~~~~~~~~~~~#

# Save list of mum-pup pairs per year
# openxlsx::write.xlsx(AGP_list[[1]], 
#                      here("Data", "Processed", "Paternity", paste0(year[1], "_m-p_mat.xlsx")))
# openxlsx::write.xlsx(AGP_list[[2]], 
#                      here("Data", "Processed", "Paternity", paste0(year[2], "_m-p_mat.xlsx")))
# openxlsx::write.xlsx(AGP_list[[3]], 
#                      here("Data", "Processed", "Paternity", paste0(year[3], "_m-p_mat.xlsx")))
# openxlsx::write.xlsx(AGP_list[[4]], 
#                      here("Data", "Processed", "Paternity", paste0(year[4], "_m-p_mat.xlsx")))

# Save mum-pup pairs all 4 years together
all_AGP <- bind_rows(AGP_list)
openxlsx::write.xlsx(all_AGP, 
                     here("Data", "Processed", "Paternity", "2017-20_m-p_mat.xlsx"))

# Save list of males
openxlsx::write.xlsx(AGM_gen, #%>% 
                     # mutate(across(everything(), ~replace_na(.x, 0))),
                     here("Data", "Processed", "Paternity", "2015-20_males_mat.xlsx"))

