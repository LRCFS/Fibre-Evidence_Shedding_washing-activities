###########################################################################

# Enhancing forensic analysis: introducing a novel method for assessing the shedding capacity of garments
# Virginie Galais(1)*, Stephanie Wilson(2), Patricia Dugard(1), Chris Gannicliffe(3), Bronagh Murphy(2), Niamh Nic Daéid(1), Hervé Ménard(1) 
# (1) Leverhulme Research Centre for Forensic Science, Department of Science and Engineering, University of Dundee, Dundee, DD1 4HN, UK
# (2) Centre for Forensic Science, Department of Pure and Applied Chemistry, University of Strathclyde, G1 1XW, UK
# (3) Scottish Police Authority Forensic Services, Aberdeen Laboratory, Aberdeen, AB24 5EQ, UK
# * Correspondence: vgalais001@dundee.ac.uk 

# Keywords: Forensic science, Fiber, Shedding, Washing, Transfer, Automated data collection


# Website: https://github.com/LRCFS/
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

###########################################################################
# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                library requirement                #####
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
library(patchwork)
library(FSA)
library(Matrix)
library(rstatix)
library(PMCMRplus)

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
source("Code/2 - Analysis - Manual pressure.R")
source("Code/3 - Analysis - Shedding.R")
source("Code/4 - Analysis - Transfer area.R")
source("Code/5 - Analysis - Shedding VS Transfer.R")

