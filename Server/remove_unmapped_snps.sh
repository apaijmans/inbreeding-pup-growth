#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=8G
#$ -N remove_unmapped_snps.sh
#$ -e /grp/moleecol/Anneke/SNPs/out/unmapped_snps.err
#$ -o /grp/moleecol/Anneke/SNPs/out/unmapped_snps.out
#$ -M paijmans@cebitec.uni-bielefeld.de
#$ -m e

/grp/moleecol/Anneke/bin/plink \
--file /grp/moleecol/Anneke/SNPs/data/processed/Rebeccas_Samples_Mendel_OriginalPedigree_hw \
--exclude /grp/moleecol/Anneke/SNPs/data/raw/snps2remove.txt \
--out /grp/moleecol/Anneke/SNPs/data/processed/Rebeccas_Samples_Mendel_OriginalPedigree_hw_mapped \
--recode --allow-extra-chr --debug

