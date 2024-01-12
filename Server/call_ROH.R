# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: ROH_calling
#
# Purpose: RAN ON SERVER!!
# 
# In this script I call ROH using PLINK
# 
#
# Date: 2023-10-06
# -----------------------------------------------------------


library(tidyverse)
library(data.table)



#~~~~~~~~~~~~~~~~~~#
#  Calling ROH  ####
#~~~~~~~~~~~~~~~~~~#

# Here, I call ROH using only the SNPs that mapped to autosomes with their updated positions.
# Parameter settings were the same as the ones used by Emily Humble in the G3 paper.

# Run PLINK from command line using qsub
system("qsub /grp/moleecol/Anneke/SNPs/jobs/emilys_params.sh")



#~~~~~~~~~~~~~~~~~~~~~#
#  Calculate Froh  ####
#~~~~~~~~~~~~~~~~~~~~~#

# New genome length: 2.528 Gb in total (2,382Gb when excluding unplaced scaffolds, i.e.: focusing only on the 18 assembled chromosomes)
total_genome_length <- 2276403716 # total genome length autosomes only

roh <- fread("data/processed/SNPs/roh-emilys_params_autosomes.hom.indiv")
hist(roh$KB/(total_genome_length/1000))

# Summary number of ROH segments per individual
summary(roh$NSEG)

Froh <- roh %>%
  dplyr::mutate(froh = KB/(total_genome_length/1000))

summary(Froh$froh)
hist(Froh$froh)

# Froh density distribution
roh_plot <- ggplot(Froh, aes(froh, fill = as.factor(Run))) +
  geom_histogram(aes(y=..density..),  position="identity", alpha=0.9, col = "grey33", fill = "grey33") +
  # geom_density(alpha=0.6, col = "grey33", fill = "grey33") + #3262AB
  labs(y = "Density", x = expression(italic(F["ROH"]))) +
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") 

roh_plot



#~~~~~~~~~~~~~~~~~~~~~~~#
#  Save Froh values  ####
#~~~~~~~~~~~~~~~~~~~~~~~#

write.table(Froh[,c(2, 1, 7)],
            "data/processed/SNPs/Froh_autosomes.txt",
            quote = F,
            row.names = F)
