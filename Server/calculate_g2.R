

# Load libraries
library(here)
library(readxl)
library(tidyverse)
library(inbreedR)


# Load SNP data
snp_data <- data.table::fread(here("data", "processed", "NEW_POS_Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes.raw"))

#str(snp_data)


# Convert data to inbreedR input
snp_genotypes <-  snp_data[,-c(1:6)]
snp_ids <- snp_data %>% select(2)

# .raw is encoded such that the SNP has _x with x being the minor allele. 0 means 0 counts of the minor allele, 1 means 1 count and 2 means two.
# eg SNP_G 0 = CC, 1 = GC or CG, 2 = GG
# So, 1 is het, 0 or 2 is hom

# convert all 2s into 0s, so that all homozygotes are 0 and heterozygotes are 1
snp_genotypes[snp_genotypes == 2] <- 0

# Check if data is in right format for InbreedR
check_data(snp_genotypes, num_ind = nrow(snp_ids), num_loci = length(snp_genotypes))

# Calculate g2 using 39 loci
g2_snp_geno <- g2_snps(snp_genotypes, nperm = 1000, nboot = 1000, CI = 0.95, parallel = T, ncores = 8)

# As g2 calculation takes a while, save the result as .R file
save(g2_snp_geno, file = here("data", "processed", "g2_snps.RData"))




## SSB individuals only

# Load SNP data
snp_data <- data.table::fread(here("data", "processed", "NEW_POS_Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes.raw"))

# Keep only SSB individuals
snp_data_SSB <- snp_data %>%
  filter(!grepl("^F", IID) & !grepl("^N", IID) & !grepl("^C", IID))

#view(snp_data_SSB %>% select(FID, IID))

#str(snp_data_SSB)


# Convert data to inbreedR input
snp_genotypes_SSB <-  snp_data_SSB[,-c(1:6)]
snp_ids_SSB <- snp_data_SSB %>% select(2)

# .raw is encoded such that the SNP has _x with x being the minor allele. 0 means 0 counts of the minor allele, 1 means 1 count and 2 means two.
# eg SNP_G 0 = CC, 1 = GC or CG, 2 = GG
# So, 1 is het, 0 or 2 is hom

# convert all 2s into 0s, so that all homozygotes are 0 and heterozygotes are 1
snp_genotypes_SSB[snp_genotypes_SSB == 2] <- 0

# Check if data is in right format for InbreedR
check_data(snp_genotypes_SSB, num_ind = nrow(snp_ids_SSB), num_loci = length(snp_genotypes_SSB))

# Calculate g2 using 39 loci
g2_snp_geno_SSB <- g2_snps(snp_genotypes_SSB, nperm = 1000, nboot = 1000, CI = 0.95, parallel = T, ncores = 8)

# As g2 calculation takes a while, save the result as .R file
save(g2_snp_geno_SSB, file = here("data", "processed", "g2_snps_SSB.RData"))


