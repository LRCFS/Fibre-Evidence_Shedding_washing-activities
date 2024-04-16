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
library(gridExtra)
library(grid)
library(devtools)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(corrplot)
library(matrixStats)
library(corrplot)
library(MASS)
library(psych)
library(car)
library(multcomp)
library(plotly)
library(patchwork)
library(FSA)
library(Matrix)

#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")
source("Functions/posthocTGH.R")
source("Functions/ReturnEveryNthLabel.R")
source("Functions/get_formula.R")
source("Functions/multkw.R")

#############################################################
#####                Folder & Files                     #####
#############################################################

# where the generated figures are saved, create folder if not existing
# dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.
Results.dir <- "Results/"

#############################################################
#####                       Codes                       #####
#############################################################
# This codes can be run subsequently
source("Code/1 - Data upload.R")
#source("Code/2 - Analysis - Impact test.R")
source("Code/3 - Analysis - Shedding.R")
source("Code/4 - Analysis - Transfer area.R")

