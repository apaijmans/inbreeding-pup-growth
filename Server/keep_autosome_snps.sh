#!/bin/bash
#SBATCH --job-name=keep_autosome_snps.sh
#SBATCH --output=/grp/moleecol/Anneke/SNPs/out/keep_autosome_snps.out
#SBATCH --error=/grp/moleecol/Anneke/SNPs/out/keep_autosome_snps.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --qos=normal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=paijmans@cebitec.uni-bielefeld.de

/grp/moleecol/Anneke/bin/plink \
--file /grp/moleecol/Anneke/SNPs/data/processed/Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped \
--exclude /grp/moleecol/Anneke/SNPs/data/raw/snps2remove_non_autosome.txt \
--out /grp/moleecol/Anneke/SNPs/data/processed/Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes \
--recode --allow-extra-chr --debug