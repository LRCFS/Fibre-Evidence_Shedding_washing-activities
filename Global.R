# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(RColorBrewer)
library(ggpubr)
library(rstatix)
library(lattice)
library(psych)
library(gridExtra)
library(grid)
library(diptest)
library(LaplacesDemon)
library(devtools)
library(cutoff)
library(matrixStats)
library(overlapping)
library(plotly)
library(FSA)
library(multcomp)
library(ggrepel)
library(car)

#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")
source("Functions/posthocTGH.R")
source("Functions/ReturnEveryNthLabel.R")
source("Functions/get_formula.R")

#############################################################
#####                Folder & Files                     #####
#############################################################

# where the generated figures are saved, create folder if not existing
# dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.
Results.dir <- "Results/"

#############################################################
#####                       Codes                       #####
#############################################################

# This codes can be run subsequently or independently as each create necessary outputs for the next codes.  
#source("Code/Data upload.R")