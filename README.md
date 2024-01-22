# Inbreeding depression for birth mass, survival and growth in Antarctic fur seal pups

This repository contains the scripts and files necessary to run the analysis done in the manuscript "Little evidence for inbreeding depression for birth mass, survival and growth in Antarctic fur seal pups". 

The raw data will become available on Zenodo (add link). This repository contains all subsequently generated files. Most datasets which are produced along the way are already saved in subfolders, so that the analysis can be started at any point.


## Main analysis and models
To run the analyses, please download the "Rcode" and "Data" folder. The folder named "Rcode" contains 12 scripts, which are named 1_ to 5_ . These scripts can be run a standard desktop machine. It also contains a custom made function ("custom_forest_plot.R"), a custom ggplot theme ("anneke_theme.R") and the Rmarkdown script ("Rmarkdown-inbreeding-pup-growth.Rmd") that generates the Rmarkdown pdf document.

## Maternity analysis
Maternity analysis was done using the NEWPAT excel macro. Scripts 1a and 1b generate the input for this macro. The data was copy pasted into the macro, which was then run ("NewPat50loci_NEW_2017-2020.xls" and "NewPat50loci_NEW_FWB.xls"). The results were then used to check the microsatellite genotyping for any scoring errors ("NewPat50loci_NEW_2017-2020_check_mismatches.xls" and "NewPat50loci_NEW_FWB_check_mismatches.xls"). Any scoring errors were manually corrected in the microsatellite data resulting in the file "msats_growth_individuals_after_checks.xlsx". This file was then used as input in the scripts 1c and 1d to rerun the maternity analysis to identify genetic mothers ("NewPat50loci_NEW_2017-2020_after_checks.xls" and "NewPat50loci_NEW_FWB_after_checks.xls"). The last two files were then used as input for script 1e.

## Workflow SNP array data (server)
Some of the analyses are computationally intensive and/or were run on a separate server. This is the case for the analyses of the SNP array data, ie data filtering in PLINK, ROH calling and calculation of g2 (scripts and data can be found in the folder "Server"). The scripts in these folders should run on a server with sufficient computing power and memory. Here, the workflow is as follows:
1. the "update_PLINK_files.R" script is the main script that calls most of the bash scripts given in the same folder. This script filters and updates the SNP array data.
2. the "call_ROH.R" script calls another bash script given in the same folder to call the ROH in PLINK, then calculates Froh.
3. the "calculate_g2.R" script calculates g2 for the SNP data.


