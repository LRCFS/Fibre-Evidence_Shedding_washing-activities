# Enhancing forensic analysis: introducing a novel method for assessing the shedding capacity of garments
[![DOI](https://zenodo.org/badge/639376806.svg)](https://doi.org/10.5281/zenodo.13907730)

This repository contains R scripts used for analysing the data from a study investigating the shedding of fibres from garments during washing activities. The aim of the study is to understand the relationship between washing activities and the amount of fibres released into wastewater, as well as fibres transferred between garments. The repository is structured to allow for reproducible analysis and figure generation, providing insight into forensic textile analysis.

0 - Global Script (Global.R)
This is the primary script that needs to be run first. It automatically calls and executes all the other scripts in the correct sequence:
Data upload
Manual pressure analysis
Shedding analysis
Transfer area analysis
Shedding vs transfer comparison
Make sure to start by running the Global.R script to ensure the correct data is loaded and processed. This will handle the automatic execution of all other scripts, ensuring the analyses are performed in the proper order.


The repository contains the following R scripts, each focusing on a different aspect of the study:

1 - Data Upload.R:
Prepares and exports all necessary data for the study.
Includes data from the Manual Pressure experiments, Impact Test, Repeated Contact data, Shedding data from washed garments and the Transfer experiments.
The data exported from this script will be used in subsequent analysis steps.

2 - Analysis - Manual Pressure.R:
This script generates figures for the following tests: Hand pressure test,Impact test and Repeated contact.
The script also saves the statistical tables relevant to these tests.

3 - Analysis - Shedding.R:
The script generates figures for shedding data across different experimental series.
The script also includes comparisons between various washing conditions and outputs statistical results based on the data.

4 - Analysis - Transfer Area.R:
This script generates figures related to the Transfer during washing series.
The script also includes steps for the statistics: Calculation and output of statistical tables based on fibre transfer data.

5 - Analysis - Shedding vs Transfer.R:
This script generates figures related to the comparison between Shedding and Transfer experiments
