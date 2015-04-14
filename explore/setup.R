### setup.R --- 
## Filename: setup.R
## Description: 
## Author: Noah Peart
## Created: Sun Apr 12 17:04:02 2015 (-0400)
## Last-Updated: Mon Apr 13 20:51:10 2015 (-0400)
##           By: Noah Peart
######################################################################
## source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")
require(plyr)
require(dplyr)
require(ggplot2)

if (!file.exists("~/work/hoods/explore/temp"))
    dir.create("~/work/hoods/explore/temp")  # store neighbor matrices
if (!file.exists("~/work/hoods/explore/temp/pp.rds") |
    !file.exists("~/work/hoods/explore/temp/tp.rds") |
    !file.exists("~/work/hoods/explore/temp/nm1.rds")) {
    source("~/work/hoods/explore/remake.R")
} else {
    pp <- readRDS("~/work/hoods/explore/temp/pp.rds")
    tp <- readRDS("~/work/hoods/explore/temp/tp.rds")
}
