#!/bin/bash
#SBATCH --job-name=emilys_params.sh
#SBATCH --output=/grp/moleecol/Anneke/SNPs/out/roh_snps_aut.out
#SBATCH --error=/grp/moleecol/Anneke/SNPs/out/roh_snps_aut.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --qos=normal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=paijmans@cebitec.uni-bielefeld.de

/grp/moleecol/Anneke/bin/plink \
--file /grp/moleecol/Anneke/SNPs/data/processed/NEW_POS_Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped_autosomes \
--aec \
--homozyg-window-snp 20 \
--homozyg-window-het 1 \
--homozyg-snp 20 \
--homozyg-kb 1000 \
--homozyg-density 100 \
--homozyg-gap 1000 \
--homozyg-window-missing 5 \
--homozyg-het 1 \
--out /grp/moleecol/Anneke/SNPs/data/processed/roh-emilys_params_autosomes \
--debug
