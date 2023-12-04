# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 3_data_exploration_filtering
#
# Purpose: This script is used to filter the data:
# - remove mismatching mothers
# - add a column with survival information
# - remove pusp with missing survival data
# In addition some basic data exploration is done.
#
# Date: 2023-12-01
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Load growth data (incl FWB individuals)  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

pup_data <- openxlsx::read.xlsx(here("Data", "Processed", "growth_sMLH_msats.xlsx"), detectDates = T)

str(pup_data)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Data filtering - mismatching mums  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ How many mismatching mums?
pup_data %>% 
  filter(!is.na(gen_mum)) %>%
  group_by(gen_mum) %>%
  summarise(n = n()) %>%
  mutate(Tot = sum(n),
         Freq = n/sum(n)) %>%
  ungroup() # 5% mismatching mum-pup pairs (30/577)

## ---- filtering_mums --------

#~~ Remove mums that are not a genetic match, but keep pups (we can still use them in the analysis without mums)
# This means we also have to remove mum birth year and sMLH for the mismatching mums
pup_data <- pup_data %>%
  mutate(ID_Mum = ifelse(gen_mum == "match", ID_Mum, NA)) %>%
  mutate(uniqueID_mum = ifelse(gen_mum == "match", uniqueID_mum, NA)) %>%
  mutate(MumBirthYear = ifelse(gen_mum == "match", MumBirthYear, NA)) %>%
  mutate(Mum_Age = ifelse(gen_mum == "match", Mum_Age, NA)) %>%
  mutate(sMLH_msat39_mum = ifelse(gen_mum == "match", sMLH_msat39_mum, NA)) %>%
  select(-c(gen_mum, MISMATCHES))

##---- chunk_end

# view(pup_data %>% filter(!is.na(ID_Mum) & is.na(sMLH_msat39_mum))) # mainly mums with more than 4 gaps, so therefore no sMLH calculated
nrow(pup_data %>% filter(is.na(ID_Mum))) # 669 pups without mums (or mum removed due to mismatches)
nrow(pup_data %>% filter(!is.na(ID_Mum))) # 577 with known mum (but not necessary also sMLH for mum)

#~~ For how many mums do we have birth years?
pup_data %>%
  filter(!is.na(ID_Mum)) %>%
  group_by(Year) %>%
  summarise(TotalMums = n(),
            MumsNoBirthyear = sum(is.na(MumBirthYear), na.rm=F),
            rel_freq_miss_mum_age = sum(is.na(MumBirthYear), na.rm=F) / n()) %>%
  ungroup()
# So a maximum of 44% of mums/year are missing birth year info
# All years combined 29% of the pups' mums are missing birth year info

# Number of unique mums with missing birth year
pup_data %>%
  filter(!is.na(ID_Mum)) %>%
  select(ID_Mum, MumBirthYear) %>%
  distinct() %>%
  summarise(TotalMums = n(),
            MumsNoBirthyear = sum(is.na(MumBirthYear), na.rm=F),
            rel_freq_miss_mum_age = sum(is.na(MumBirthYear), na.rm=F) / n())
# 319 unique mums, 120 are missing birth year (37%)
# This number is slightly different because here we look at unique mums



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Data filtering - survival  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~ Add binary survival column
nrow(pup_data %>% filter(!is.na(Pup_Death))) # 209 pups with a date of death
nrow(pup_data %>% filter(!is.na(Cat_Death))) # 225 pups with a death category
nrow(pup_data %>% filter(!is.na(Cat_Death) | !is.na(Pup_Death))) # 226. All individuals that died on FWB have no death date, because I did not add that info in the data wrangling steps

nrow(pup_data %>% filter(!is.na(Pup_TagWeight) & (!is.na(Cat_Death) | !is.na(Pup_Death)))) #46 were weighted at tagging, but died later

## ---- survival_data --------

pup_data <- pup_data %>% 
  mutate(Survival = ifelse(!is.na(Cat_Death) | !is.na(Pup_Death), "0", 
                           ifelse(!is.na(Pup_TagWeight) & !is.na(Pup_Death), "0", 
                                  ifelse(!is.na(Pup_TagWeight) & is.na(Pup_Death), "1", NA)))) %>%
  mutate(Survival = as.factor(Survival))

# All pups that do not have a tagging weight AND also no death date will now have an NA in the Survival column.
# These will be removed: we wont be able to use them in the growth model nor the survival model (as we do not know whether they were dead or alive). 
# So for consistency, we will also not use them in the birth mass model (n=231)
# nrow(pup_data %>% filter(is.na(Survival)))
# All pups with a 2nd weight and no death date are assumed to have survived at least until the end of the season

pup_data <- pup_data %>%
  filter(!is.na(Survival))

##---- chunk_end



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Explore age at death  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Total number of pups that died with known age of death
n_dead <- nrow(pup_data %>% filter(Survival == 0) %>% filter(!is.na(Age_Death)))

# Total number of pups that died after tagging
nrow(pup_data %>% filter(Survival == 0 & !is.na(Pup_TagWeight))) # 46

#~~ Calculate accumulative proportion of animals that died at a specific age (ie at age 7, x percent of pups have died etc.)
prop_dead <- pup_data %>% 
  select(ID_Pup, Age_Death) %>%
  filter(!is.na(Age_Death)) %>% 
  group_by(Age_Death) %>%
  count() %>%
  within(., acc_sum <- cumsum(n)) %>%
  mutate(acc_prop = acc_sum / n_dead)

#~~ After how many days did 90% of the pups die?
above_threshold <- prop_dead %>%
  filter(acc_prop > 0.9)

min(above_threshold$Age_Death, na.rm = TRUE) # 35 days

#~~ Make figure with 90% threshold
source(here("Rcode", "anneke_theme.R"))

col1 <- "#872ca2"
col2 <- "#f6a97a"

lab <- paste0("90% died in the first ", min(above_threshold$Age_Death, na.rm = TRUE), " days")

ggplot(pup_data, aes(x = Age_Death)) +
  geom_histogram(fill= col2) +
  geom_vline(aes(xintercept = min(above_threshold$Age_Death, na.rm = TRUE)), linetype = "dashed", color = col1) +
  xlab("Age at death (days)") +
  annotate(x = min(above_threshold$Age_Death, na.rm = TRUE), y = +Inf, 
           color = col1, 
           label= lab, 
           vjust=2, hjust = -0.05, 
           geom="text") +
  theme_anneke()

# ggsave("Figs/age_pup_mortality.jpg", width = 7, height = 3.5)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Explore death category  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

death_cat <- pup_data %>% 
  filter(Survival==0) %>%
  group_by(Cat_Death) %>% 
  count() %>% 
  ungroup() 

ggplot(death_cat, aes(x = Cat_Death, y = n)) +
  geom_col() +
  scale_x_discrete(name ="Pup death category", 
                   labels=c("Infection","Predation", "Still born", "Starvation", "Trauma", "Unknown", "NA")) +
  ylab("Number of pups") +
  theme_anneke() +
  theme(axis.text.x = element_text(angle = 45))

# ggsave("Figs/cat_pup_mortality.jpg", width = 7, height = 5)

## ---- col_cat --------

#~~ Fix column categories
pup_data <- pup_data %>%
  mutate(Pup_Sex = as.factor(Pup_Sex)) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Survival = as.factor(Survival))

##---- chunk_end

#~~ Save filtered data

openxlsx::write.xlsx(pup_data,
                     here("Data", "Processed", "filtered_pup_survival.xlsx"), quote = F,
                     row.names = F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Explore missing data  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Records will be incomplete: some pups will have missing data for 1 or more variable
# Here we explore this a little

nrow(pup_data %>% filter(is.na(Pup_BirthWeight))) # 98 pups with missing birth weights
nrow(pup_data %>% filter(is.na(Pup_TagWeight))) # 180 pups without tag weight
nrow(pup_data %>% filter(is.na(WeightGain))) # 243 pups without weight gain info
nrow(pup_data %>% filter(!is.na(Pup_Death))) # 209 pups that died (which will also not have weight gain info)

# Keep only pups that survived
pup_alife <- pup_data %>% 
  filter(Survival == "1") %>%
  select(-c(Pup_Death, Survival))
# 858 obs

pup_alife %>% filter(is.na(Age_Tag)) # zero pups without age info left



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Complete data incl maternal info  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Only pups with complete data will be used in the models. Here, I explore the complete data incl maternal effects

# Select those pups that have complete data for the following variables
complete.pups <- pup_data %>%
  drop_na(c(Survival, Pup_BirthWeight, sMLH_msat39_pup, Pup_Sex, Year, sMLH_msat39_mum, Mum_Age, ID_Mum))


#~~ Count number of unique mums and unique pups in the model
length(unique(complete.pups$ID_Mum)) #180 unique mums

complete.pups %>% count(ID_Mum) %>% count(n)
# n pups  mums
# 1 1 pup 72
# 2 2 pup 63
# 3 3 pup 36
# 4 4 pup  9
#
# So 108 mothers had more than 1 pup

length(unique(complete.pups$uniqueID_pup)) #342 unique pups with mothers

#~~ Exploration of weight at birth 
complete.pups %>% 
  summarise(mean(Pup_BirthWeight, na.rm=T),
            min(Pup_BirthWeight, na.rm=T),
            max(Pup_BirthWeight, na.rm=T))
# Mean = 4.8, min = 3.2, max = 7)

# Per season
ggplot(complete.pups, aes(x = Year, y = Pup_BirthWeight)) +
  geom_boxplot()

#~~ Exploration of age at tagging 
complete.pups %>% 
  filter(Survival == 1) %>%
  summarise(mean(Age_Tag, na.rm=T),
            min(Age_Tag, na.rm=T),
            max(Age_Tag, na.rm=T))
# Mean = 47, min = 21, max = 81)

# Per season
ggplot(complete.pups, aes(x = Year, y = Age_Tag)) +
  geom_boxplot()


#~~ Exploration of pup sex ratio per year
sex_ratio <- complete.pups %>% 
  filter(!is.na(Pup_Sex)) %>%
  group_by(Year,Pup_Sex) %>%
  count() %>%
  group_by(Year) %>%
  mutate(per =  n/sum(n))

# Make figure
source(here("Rcode", "anneke_theme.R"))

col1 <- "#872ca2"
col2 <- "#f6a97a"

ggplot(sex_ratio, aes(x = Year, y = per, fill = Pup_Sex, label = scales::percent(round(per, 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(col2,col1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", show.legend = FALSE) +
  scale_y_continuous(name = "Sex ratio pups born", label = scales::percent) +
  scale_x_discrete(name ="Year", labels=c("2018","2019","2020", "2021")) +
  labs(fill="Pup sex") +
  theme_anneke()

# ggsave("Figs/sex_ratio_pups_born.jpg", width = 5, height = 3.5)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Complete data excl maternal info  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Only pups with complete data will be used in the models. Here, I explore the complete data excl maternal effects

# Select those pups that have complete data for the following variables
complete.pups2 <- pup_data %>%
  drop_na(c(Survival, Pup_BirthWeight, sMLH_msat39_pup, Pup_Sex, Year))


#~~ Exploration of weight at birth 
complete.pups2 %>% 
  summarise(mean(Pup_BirthWeight, na.rm=T),
            min(Pup_BirthWeight, na.rm=T),
            max(Pup_BirthWeight, na.rm=T))
# Mean = 4.8, min = 2.5, max = 7.7)

# Per season
ggplot(complete.pups2, aes(x = Year, y = Pup_BirthWeight)) +
  geom_boxplot()

#~~ Exploration of age at tagging of surviving pups
complete.pups2 %>%
  filter(Survival == 1) %>%
  summarise(mean(Age_Tag, na.rm=T),
            min(Age_Tag, na.rm=T),
            max(Age_Tag, na.rm=T))
# Mean = 49, min = 12, max = 89)

# Per season
ggplot(complete.pups %>% filter(Survival == 1), aes(x = Year, y = Age_Tag)) +
  geom_boxplot()


#~~ Exploration of pup sex ratio per year
complete.pups2 %>% 
  filter(!is.na(Pup_Sex)) %>%
  group_by(Year,Pup_Sex) %>%
  count() %>%
  group_by(Year) %>%
  mutate(per =  n/sum(n))

ggplot(complete.pups2 %>% 
         filter(!is.na(Pup_Sex)) %>%
         group_by(Year,Pup_Sex) %>%
         count() %>%
         group_by(Year) %>%
         mutate(per =  n/sum(n)), 
       aes(x = Year, y = per, fill = Pup_Sex, label = scales::percent(round(per, 2)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(col2,col1)) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", show.legend = FALSE) +
  scale_y_continuous(name = "Sex ratio pups born", label = scales::percent) +
  scale_x_discrete(name ="Year", labels=c("2018","2019","2020", "2021")) +
  labs(fill="Pup sex") +
  theme_anneke()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Some explorative plots  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Histogram of growth for all pups
ggplot(complete.pups2, aes(x=WeightGain)) +
  geom_histogram()
# Looks relatively normally distributed but a bit more weight to the left side of the figure

# Histogram weight at tagging for all pups
ggplot(complete.pups2, aes(x=Pup_TagWeight)) +
  geom_histogram()

# Age at taggin ~ Age, split by sex
ggplot(complete.pups2, aes(x=Age_Tag, y=Pup_TagWeight, color=Pup_Sex)) +
  geom_point() +
  geom_smooth(method='lm')
# Males seem to grow faster

# Age at tagging ~ sMLH, split by sex
ggplot(complete.pups2, aes(x=sMLH_msat39_pup, y=Pup_TagWeight, color=Pup_Sex)) +
  geom_point() +
  geom_smooth(method='lm')
# Looks very similar for the sexes, would not expect an interaction

ggplot(complete.pups2, aes(x=Pup_Sex, y=sMLH_msat39_pup)) +
  geom_boxplot()
# sMLH does not look different between sexes

