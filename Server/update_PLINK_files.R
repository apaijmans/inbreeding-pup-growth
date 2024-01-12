# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: update_PLINK_files
#
# Purpose: RAN ON SERVER!!
# 
# The SNP positions are based on an older version of the genome. 
# By mapping the SNP + flank to the new version, we extracted new positions for the majority of SNPs (done by David Vendrami)
# In this code, I will update the PLINK files with the new mapping information, so we can later call ROH.
# To do this, I first removed the SNPs that did not map (based on list of unmapped variants received from David).
# I then removed all SNPs that are not located on the autosomes.
# Finally, I added the new position information.
#
# Date: 2023-10-06
# -----------------------------------------------------------


library(tidyverse)
library(data.table)



#~~~~~~~~~~~~~~~~~~~~#
#  HWE filtering  ####
#~~~~~~~~~~~~~~~~~~~~#

# Run PLINK from command line using qsub
system("qsub /grp/moleecol/Anneke/SNPs/jobs/HWE_filter.sh")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Remove unmapped SNPs  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# There are some variants that do not map
# First: generate simple text file with ids from unmapped variants:
unmapped <- read.table("data/raw/SNParray_UNmappedToHaplotype1.map")

write.table(unmapped[,2], "data/raw/snps2remove.txt", col.names = F, row.names = F, quote = F)

# Then, run PLINK from command line using qsub to remove the variants
system("qsub /grp/moleecol/Anneke/SNPs/jobs/remove_unmapped_snps.sh")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Keep only autosomal SNPs  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load new positions SNPs, which I need to identify which SNPs are on autosomes and which ones not
new_pos <- read.table("data/raw/SNParray_MappedToHaplotype1.map") %>%
  rename(chr=V1, id=V2, posM=V3, pos=V4) 

# Remove SNPs that are not on autosomes
snps_not_autosomes <- new_pos %>%
  filter(!grepl("^mscaf", chr) | chr == "mscaf_a1_x") 

# Generate simple text file containing ids of variants that are not on the autosomes and can therefore be removed:
write.table(snps_not_autosomes[,2], "data/raw/snps2remove_non_autosome.txt", col.names = F, row.names = F, quote = F)

# Run PLINK from command line using qsub to remove variants
system("sbatch /grp/moleecol/Anneke/SNPs/jobs/keep_autosome_snps.sh")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Add new position information  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load plink map
plink_files <- "Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes"

# Now match the new contigID with the .map file and replace chr ids. 

# Load map file and annotation file, so that we can match them based on SNP id
old_map <- fread(paste0("data/processed/", plink_files, ".map")) %>%
  rename(old_chr=V1, id=V2, old_posM=V3, old_pos=V4)

# # Check if all ids are unique
# anyDuplicated(map_file$id) # 0, so yes

new_map <- left_join(old_map, new_pos, by = "id") %>%
  select(chr, id, posM, pos) 

# IMPORTANT! Check order is the same!
identical(new_map$id, old_map$id)

chr <- new_map %>% group_by(chr) %>% tally() 



#~~~~~~~~~~~~~~~~#
#  Save data  ####
#~~~~~~~~~~~~~~~~#

# Write map file
write.table(new_map, paste0("data/processed/NEW_POS_", plink_files, ".map"),
            quote = F, row.names = F, col.names = F)


# Make copy of ped file with new name so that it is easier to run in PLINK later
ped_file <- fread(paste0("data/processed/", plink_files, ".ped"))

write.table(ped_file, paste0("data/processed/NEW_POS_", plink_files, ".ped"),
            quote = F, row.names = F, col.names = F)

# Create .raw file to calculate sMLH/g2
system(paste0("/grp/moleecol/Anneke/bin/plink ", 
              "--file /grp/moleecol/Anneke/SNPs/data/processed/NEW_POS_Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes ",
              "--out /grp/moleecol/Anneke/SNPs/data/processed/NEW_POS_Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes ",
              "--recode A --allow-extra-chr --debug"))

