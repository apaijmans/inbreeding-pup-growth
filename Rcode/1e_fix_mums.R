# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 1e_fix_mums
#
# Purpose: This script removes mum-pup pairs that are not a genetic match 
# according to the NEWPAT maternity analysis from the growth dataframe
#
# Date: 2023-12-03
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)
library(inbreedR)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Load growth data (incl FWB individuals)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

pup_data <- openxlsx::read.xlsx(here("Data", "Processed", "pup_growth_2017-2020.xlsx"), detectDates = T)

# pup_data <- pups %>%
#   filter(!grepl("^AGPC", ID_Pup) & !grepl("^AGFC", ID_Pup))

which(is.na(as.numeric(pup_data$Age_Tag)) != is.na(pup_data$Age_Tag))
pup_data$Age_Tag <- as.numeric(pup_data$Age_Tag)

which(is.na(as.numeric(pup_data$Age_Death)) != is.na(pup_data$Age_Death))
pup_data$Age_Death <- as.numeric(pup_data$Age_Death)


str(pup_data)

#~~ Check for duplicates
pup_data %>% filter(ID_Pup %in% unique(.[["ID_Pup"]][duplicated(.[["ID_Pup"]])])) # 0

# view(pup_data %>% filter(is.na(ID_Mum) & !is.na(uniqueID_mum))) # zero
# view(pup_data %>% filter(!is.na(ID_Mum) & is.na(uniqueID_mum))) # zero



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Add mum-pup relatedness based on NEWPAT macro SSB  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Load the mismatching mum-pup pairs

mismatches_allowed <- 1

mismatches_after_checks <- here("Data", "Processed", "Paternity", "NewPat50loci_NEW_2017-2020_after_checks.xls")

mum_pup_mismatch <- read_excel(mismatches_after_checks) %>%
  select(MUM=...1, PUP=...2, MISMATCHES=...44)

row1 <- which(grepl("Female-offspring mismatches", mum_pup_mismatch$MUM)) + 1

rowlast <- which(grepl("Finding fathers for offspring", mum_pup_mismatch$MUM)) - 2

mum_pup_mismatch <- mum_pup_mismatch[row1:rowlast, ] %>%
  mutate(MISMATCHES = as.numeric(MISMATCHES)) %>%
  mutate(gen_mum = ifelse(MISMATCHES > mismatches_allowed, "mismatch", NA))

#~~ Load the matching mum-pup pairs via loop

mum_pup_match <- read_excel(mismatches_after_checks,
                            sheet="Input1",
                            skip=1) %>%
  select(id, status) %>%
  filter(status != "M") %>%
  mutate(dummy=rep(1:(nrow(.)/2), each=2)) %>%
  pivot_wider(., names_from = status, values_from = id) %>%
  select(MUM = "F", PUP = O)


#~~ Combine mum_pup_match and mum_pup_mismatch
mum_pup_SSB <- left_join(mum_pup_match, mum_pup_mismatch) %>%
  mutate(MISMATCHES = ifelse(is.na(MISMATCHES), 0, MISMATCHES)) %>%
  mutate(gen_mum = ifelse(is.na(gen_mum), "match", gen_mum))

# Check for duplicate pups
mum_pup_SSB %>% filter(PUP %in% unique(.[["PUP"]][duplicated(.[["PUP"]])])) # 1 duplicate, AGP18083

# AGP18083 and AGP18084 have identical genotypes, so this is most likely a sampling mistake.
# In the NEWPAT maternity test I compared AGP18083 with the mother assigned to it in the field (AGP11186), 
# as well as with the mother assigned to AGP18084 (AGP13075).
# The genetic match was with the mother AGP11186. From this I concluded that the genotype is in fact belonging to AGP18083.
# So the DNA must be indeed from AGP18083, and not AGP18084.
# Because of this I will give AGP18084 a different unique ID in the pup_data df.

pup_data[which(pup_data$ID_Pup == "AGP18084"), "uniqueID_pup"] <- "ID_0"

# Check for duplicate mums
dup_mums <- mum_pup_SSB %>% filter(MUM %in% unique(.[["MUM"]][duplicated(.[["MUM"]])])) # mothers can have multiple pups, however only 1 per year

# Check if mums have multiple pups in 1 year
dup_mums %>%
  group_by(MUM) %>%
  mutate(yearPup = substr(PUP, 1, 5)) %>%
  group_by(MUM, yearPup) %>%
  count(yearPup) %>%
  filter(n >1) # yes, AGF19009 and AGP13064



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Add mum-pup relatedness based on NEWPAT macro FWB  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

mismatches_after_checks <- here("Data", "Processed", "Paternity", "NewPat50loci_NEW_FWB_after_checks.xls")

mum_pup_mismatch <- read_excel(mismatches_after_checks) %>%
  select(MUM=...1, PUP=...2, MISMATCHES=...44)

row1 <- which(grepl("Female-offspring mismatches", mum_pup_mismatch$MUM)) + 1

rowlast <- which(grepl("Finding fathers for offspring", mum_pup_mismatch$MUM)) - 2

mum_pup_mismatch <- mum_pup_mismatch[row1:rowlast, ] %>%
  mutate(MISMATCHES = as.numeric(MISMATCHES)) %>%
  mutate(gen_mum = ifelse(MISMATCHES > mismatches_allowed, "mismatch", NA))


mum_pup_match <- read_excel(mismatches_after_checks,
                            sheet="Input1",
                            skip=1) %>%
  select(id, status) %>%
  filter(status != "M") %>%
  mutate(dummy=rep(1:(nrow(.)/2), each=2)) %>%
  pivot_wider(., names_from = status, values_from = id) %>%
  select(MUM = "F", PUP = O)

# Combine mum_pup_match and mum_pup_mismatch
mum_pup_FWB <- left_join(mum_pup_match, mum_pup_mismatch) %>%
  mutate(MISMATCHES = ifelse(is.na(MISMATCHES), 0, MISMATCHES)) %>%
  mutate(gen_mum = ifelse(is.na(gen_mum), "match", gen_mum))

# Check for duplicate pups
mum_pup_FWB %>% filter(PUP %in% unique(.[["PUP"]][duplicated(.[["PUP"]])])) # 0

# Check for duplicate mums
dup_mums <- mum_pup_FWB %>% filter(MUM %in% unique(.[["MUM"]][duplicated(.[["MUM"]])])) # mothers can have multiple pups, however only 1 per year

# Check if mums have multiple pups in 1 year
dup_mums %>%
  group_by(MUM) %>%
  mutate(yearPup = substr(PUP, 1, 5)) %>%
  group_by(MUM, yearPup) %>%
  count(yearPup) %>%
  filter(n >1) # 0

#~~ Row bind with SSB data

mum_pup <- rbind(mum_pup_SSB, mum_pup_FWB)



#~~~~~~~~~~~~~~~~~~~~~#
#  Add unique IDs  ####
#~~~~~~~~~~~~~~~~~~~~~#

#~~ Replace mum-pup info with unique ID
unique_ID <- read_excel(here("Data", "Processed", "msats_growth_individuals_after_checks.xlsx"),
                        guess_max = 16000) %>%
  select(dummyID, uniqueID) %>%
  distinct(dummyID, .keep_all = T)

mum_pup <- left_join(mum_pup, unique_ID, by = c("MUM" = "dummyID")) %>%
  rename(uniqueID_mum=uniqueID) %>%
  left_join(unique_ID, by = c("PUP" = "dummyID")) %>%
  rename(uniqueID_pup=uniqueID) %>%
  select(-c(MUM, PUP))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Add genetic match info to pup growth df  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

pup_data <- full_join(pup_data, mum_pup, by = c("uniqueID_mum", "uniqueID_pup"))

# Full join, so check for duplicates
pup_data %>% filter(ID_Pup %in% unique(.[["ID_Pup"]][duplicated(.[["ID_Pup"]])])) # 0

# Unique mums, but this may not be very informative as she can be a gen match with a pup in one year and a mismatch in the other...
nrow(pup_data %>% distinct(ID_Mum, .keep_all = T) %>% filter(!is.na(ID_Mum) & gen_mum == "match")) #313 gen matching mums (0 or 1 mismatching locus) incl FWB
nrow(pup_data %>% distinct(ID_Mum, .keep_all = T) %>% filter(!is.na(ID_Mum) & gen_mum == "mismatch")) #17 gen mismatching mums (ie more than 1 mismatching loci, not taking into account gaps) incl FWB

#~~ Percentage of mismatches
nrow(pup_data %>% filter(!grepl("^AGPC", ID_Pup)) %>% filter(!is.na(ID_Mum) & !is.na(gen_mum)))
nrow(pup_data %>% filter(!is.na(ID_Mum) & !is.na(gen_mum))) # 555 pups with a matching OR mismatching mum, 606 incl FWB

nrow(pup_data %>% filter(!grepl("^AGPC", ID_Pup)) %>% filter(!is.na(ID_Mum) & gen_mum == "match"))
nrow(pup_data %>% filter(!is.na(ID_Mum) & gen_mum == "match")) #529 gen matching mums (0 or 1 mismatching locus), 577 incl FWB

nrow(pup_data %>% filter(!grepl("^AGPC", ID_Pup)) %>% filter(!is.na(ID_Mum) & gen_mum == "mismatch"))
nrow(pup_data %>% filter(!is.na(ID_Mum) & gen_mum == "mismatch")) #26 gen mismatching mums (ie more than 1 mismatching loci, not taking into account gaps), 29 incl FWB

pup_data %>% 
  filter(!grepl("^AGPC", ID_Pup)) %>% 
  filter(!is.na(ID_Mum)) %>% 
  group_by(MISMATCHES) %>% 
  tally() %>% 
  ungroup()
# 500 SSB pups with zero mismatches with their mother, 29 with 1 mismatch

nrow(pup_data %>% filter(!is.na(ID_Mum) & MISMATCHES == 1)) # 29 mismatching mums, 32 incl FWB
nrow(pup_data %>% filter(!is.na(ID_Mum) & is.na(gen_mum))) # 11 mums where a genetic match could not be checked (eg microsat data too gappy )

pup_data %>% 
  filter(!grepl("^AGPC", ID_Pup)) %>% 
  #group_by(MISMATCHES) %>%
  summarise(TotalMums = n(),
            OneMismatch = sum(MISMATCHES==1, na.rm=F),
            ZeroMismatch = sum(MISMATCHES==0, na.rm=F),
            freqmist = sum(is.na(MumBirthYear), na.rm=F) / n()) 
  
# 1121 pups with sMLH info

nrow(pup_data %>% filter(!is.na(sMLH_msat39_pup))) # 1172 incl FWB

pup_data %>%
  filter(!is.na(ID_Mum)) %>%
  group_by(Year) %>%
  summarise(TotalMums = n(),
            MumsNoBirthyear = sum(is.na(MumBirthYear), na.rm=F),
            rel_freq_miss_mum_age = sum(is.na(MumBirthYear), na.rm=F) / n()) %>%
  ungroup()


#~~~~~~~~~~~~~~~~#
#  Save data  ####
#~~~~~~~~~~~~~~~~#

openxlsx::write.xlsx(pup_data,
                     here("Data", "Processed", "pup_growth_mums_checked.xlsx"), quote = F,
                     row.names = F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Extra: compare number of mismatches before and after checks  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Load the mismatching mum-pup pairs before the checks

mismatches_before_checks <- here("Data", "Processed", "Paternity", "NewPat50loci_NEW_2017-2020_check_mismatches.xls")

mum_pup_mismatch <- read_excel(mismatches_before_checks) %>%
  select(MUM=...1, PUP=...2, MISMATCHES=...44)

row1 <- which(grepl("Female-offspring mismatches", mum_pup_mismatch$MUM)) + 1

rowlast <- which(grepl("Finding fathers for offspring", mum_pup_mismatch$MUM)) - 2

mum_pup_mismatch <- mum_pup_mismatch[row1:rowlast, ] %>%
  mutate(MISMATCHES = as.numeric(MISMATCHES)) %>%
  mutate(gen_mum = ifelse(MISMATCHES > mismatches_allowed, "mismatch", NA))

#~~ Load the matching mum-pup pairs

mum_pup_match <- read_excel(mismatches_before_checks,
                            sheet="Input1",
                            skip=1) %>%
  select(id, status) %>%
  filter(status != "M") %>%
  mutate(dummy=rep(1:(nrow(.)/2), each=2)) %>%
  pivot_wider(., names_from = status, values_from = id) %>%
  select(MUM = "F", PUP = O)


#~~ Combine mum_pup_match and mum_pup_mismatch
mum_pup_SSB_bf <- left_join(mum_pup_match, mum_pup_mismatch) %>%
  mutate(MISMATCHES = ifelse(is.na(MISMATCHES), 0, MISMATCHES)) %>%
  mutate(gen_mum = ifelse(is.na(gen_mum), "match", gen_mum))


#~~ Make a quick figure to see how mismatches are distributed (before and after checks)
ggplot() + 
  geom_histogram(data = mum_pup_SSB_bf, aes(x = MISMATCHES, fill = "before"), alpha = 0.3, bins = max(mum_pup_SSB_bf$MISMATCHES, rm.na = T)) +
  geom_histogram(data = mum_pup_SSB, aes(x = MISMATCHES, fill = "after"), alpha = 0.3, bins = max(mum_pup_SSB$MISMATCHES, rm.na = T))

#~~ Make a quick figure to see how mismatches are distributed (before checks)
m_p_hist <- ggplot(mum_pup_SSB_bf, aes(x=MISMATCHES)) +
  geom_histogram(bins=max(mum_pup_SSB_bf$MISMATCHES)) +
  xlab("# mismatching loci mum-pup pairs before checks")

m_p_hist
# 
# # ggsave(here("Figs", "Mismatching_loci_m-p_pairs_before_checks.jpg"), m_p_hist, width = 4, height = 4)

