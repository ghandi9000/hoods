### setup.R --- 
## Filename: setup.R
## Description: create/cache neighbor matrices/datasets
## Author: Noah Peart
## Created: Sun Apr 12 17:04:02 2015 (-0400)
## Last-Updated: Fri Apr 17 14:21:20 2015 (-0400)
##           By: Noah Peart
######################################################################
## source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")
require(plyr)
require(dplyr)
require(ggplot2)

if (!file.exists("temp"))
    dir.create("temp")  # store neighbor matrices
if (!file.exists("temp/pp.rds") |
    !file.exists("temp/tp.rds") |
    !file.exists("temp/nm1.rds")) {
    source("remake.R")
} else {
    pp <- readRDS("temp/pp.rds")
    tp <- readRDS("temp/tp.rds")
}
