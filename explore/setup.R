### setup.R --- 
## Filename: setup.R
## Description: 
## Author: Noah Peart
## Created: Sun Apr 12 17:04:02 2015 (-0400)
## Last-Updated: Mon Apr 13 00:14:58 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/canopy/load_canopy.R")
require(plyr)
require(dplyr)
require(ggplot2)

## Data with estimated heights/boles
pp <- read.csv("~/work/temp/pp.csv")
tp <- read.csv("~/work/temp/transect.csv")

################################################################################
##
##                              Permanent Plots
##
################################################################################
## tidy, wide -> long
yrs <- c(86, 87, 98, 10)
cols <- grep("^STAT|^DBH|^ht[0-9]+|HTTCR|bv|PPLOT|SPLOT$|^DECM|TAG$|SPEC|ASP|ELEV|BQUDX|BQUDY|CLASS$|^canht",
             names(pp))
dat <- pp[pp$PPLOT > 3, cols]
cols <- grep("[A-Za-z]+$|.*86$|.*87$|.*98$|.*10$", names(dat))
dat <- dat[, cols]  # remove other year columns

dat <- reshape(dat, times = yrs, direction = "long",
               varying = list(
               STAT = grep("^STAT", names(dat)),
               DBH = grep("^DBH", names(dat)),
               HT = grep("^ht", names(dat)),
               BV = grep("bv", names(dat)),
               HTOBS = grep("HTTCR", names(dat)),
               CANHT = grep("canht", names(dat)),
               DECM = grep("DECM", names(dat))),
               v.names = c("STAT", "DBH", "HT", "BV", "HTOBS", "CANHT", "DECM"),
               timevar = "YEAR")
dat$YEAR <- factor(dat$YEAR)
pp <- dat[!is.na(dat$DBH), ]
pp$BA <- 0.00007854*pp$DBH*pp$DBH

################################################################################
##
##                                    MNM
##
################################################################################
source("~/work/functions/neighborhoods/rewrite/final/mnm.R")
source("~/work/functions/neighborhoods/rewrite/final/mnm-to-matrix.R")

## mnm function requires "time" variable, and lowercase column names
matDat <- pp
names(matDat) <- tolower(names(matDat))
names(matDat)[names(matDat) == "year"] <- "time"

## Target and neighbor parameters
tPars <- quote(!is.na(ba) &
               ba > 0 )
##               spec %in% c("ABBA", "BECO"))

nPars <- quote(!is.na(neighbor[["ba"]]))
##               neighbor[["ba"]] >= target[["ba"]])

nCols <- c("ba", "id", "bqudx", "bqudy", "spec")

dPars <- quote(!is.na(ba) &
               stat == "ALIVE" &
               !is.na(bqudx) &
               bqudx < 11 &
               bqudx > 0 &
               !is.na(bqudy) &
               bqudy < 11 &
               bqudy > 0 &
               pplot > 3)
## nRad <- 2

## nLst <- mnm(tPars = tPars, nPars = nPars, dPars = dPars, nCols = nCols,
##            nRad = nRad, dat = matDat, parallel=F)
## nMat <- mnm_to_matrix(nLst)

################################################################################
##
##                                 Transects
##
################################################################################
## tidy, wide -> long
yrs <- c(87, 98, 99, 10, 11)
cols <- grep("canht|^STAT|^DBH|^HT[[:digit:]]|^ht[[:digit:]]+|^bv|TRAN|TPLOT|TAG|SPEC|ASP|ELEV", names(tp))
dat <- tp[, cols]
cols <- grep("[A-Za-z]+$|87$|98$|99$|10$|11$", names(dat))
dat <- dat[, cols]  # remove other year columns
dat[, paste0("HT", c(87, 98, 10))] <- NA

dat <- reshape(dat, times = yrs, direction = "long",
               varying = list(
               STAT = grep("^STAT", names(dat)),
               DBH = grep("^DBH", names(dat)),
               BV = grep("^bv", names(dat)),
               HT = grep("^ht[[:digit:]]", names(dat)),
               HTOBS = grep("^HT[[:digit:]]", names(dat)),
               CANHT = grep("canht", names(dat))),
               v.names = c("STAT", "DBH", "BV", "HT", "HTOBS", "CANHT"),
               timevar = "YEAR")
dat$YEAR <- factor(dat$YEAR)
tp <- dat[!is.na(dat$DBH), ]
tp$BA <- 0.00007854*tp$DBH*tp$DBH

## ## Add canopy heights
## inds <- match(interaction(dat$TPLOT, dat$TRAN, dat$YEAR),
##               interaction(hh_plot$TPLOT, hh_plot$TRAN, hh_plot$time))
## dat$CANHT <- hh_plot[inds, "ht_mean"]
