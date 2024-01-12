#!/bin/bash
#$ -P fair_share
#$ -cwd
#$ -l idle=1
#$ -l vf=8G
#$ -N calc_g2.sh
#$ -e /grp/moleecol/Anneke/SNPs/out/calc_g2.err
#$ -o /grp/moleecol/Anneke/SNPs/out/calc_g2.out
#$ -M paijmans@cebitec.uni-bielefeld.de
#$ -m e

Rscript /grp/moleecol/Anneke/SNPs/code/calculate_g2.R