#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=8G
#$ -N HWE_filter.sh
#$ -e /grp/moleecol/Anneke/SNPs/out/snps_hw.err
#$ -o /grp/moleecol/Anneke/SNPs/out/snps_hw.out
#$ -M paijmans@cebitec.uni-bielefeld.de
#$ -m e

/grp/moleecol/Anneke/bin/plink \
--file /grp/moleecol/Anneke/SNPs/data/raw/Rebeccas_Samples_Mendel_OriginalPedigree \
--hardy \
--hwe 0.001 midp \
--out /grp/moleecol/Anneke/SNPs/data/processed/Rebeccas_Samples_Mendel_OriginalPedigree_hw \
--recode --allow-extra-chr --debug

