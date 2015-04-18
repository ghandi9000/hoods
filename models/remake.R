### remake.R --- 
## Filename: remake.R
## Description: Remake datasets for analysis
## Author: Noah Peart
## Created: Mon Apr 13 20:07:47 2015 (-0400)
## Last-Updated: Sat Apr 18 00:17:49 2015 (-0400)
##           By: Noah Peart
######################################################################
require(plyr)
require(dplyr)

## Data with estimated heights/boles
if (file.exists("temp/pp.csv") & file.exists("temp/transect.csv")) {
    pp <- read.csv("temp/pp.csv")
    tp <- read.csv("temp/transect.csv")
} else {
    pp <- read.csv("~/work/temp/pp.csv")
    tp <- read.csv("~/work/temp/transect.csv")
}

################################################################################
##
##                              Permanent Plots
##
################################################################################
## tidy, wide -> long
yrs <- c(86, 87, 98, 10)
cols <- grep("^STAT|^DBH|^ht[0-9]+|HTTCR|bv|PPLOT|SPLOT$|^DECM|TAG$|SPEC|ASP|ELEV|BQUDX|BQUDY|CLASS$|^canht|^CPOS",
             names(pp))
dat <- pp[pp$PPLOT > 3, cols]
cols <- grep("[A-Za-z]+$|.*86$|.*87$|.*98$|.*10$", names(dat))
dat <- dat[, cols]  # remove other year columns
dat[,paste0("BA",yrs)] <- 0.00007854 * dat[,paste0("DBH", yrs)]**2

## Growth columns
vars <- c("DBH", "ht", "bv", "canht", "HTTCR", "BA")
for (v in vars) {
    dat[,paste0("g_", v, 86)] <- (dat[,paste0(v, 98)] - dat[,paste0(v, 86)])/12
    dat[,paste0("g_", v, 87)] <- (dat[,paste0(v, 98)] - dat[,paste0(v, 87)])/11
    dat[,paste0("g_", v, 98)] <- (dat[,paste0(v, 10)] - dat[,paste0(v, 98)])/12
}

dat[,paste0("g_", vars, 10)] <- NA
dat$CPOS86 <- NA  # no crown positions measured in 86
dat <- reshape(dat, times = yrs, direction = "long",
               varying = list(
               BA = grep("^BA", names(dat)),
               gBA = grep("g_BA", names(dat)),
               STAT = grep("^STAT", names(dat)),
               DBH = grep("^DBH", names(dat)),
               gDBH = grep("^g_DBH", names(dat)),
               HT = grep("^ht", names(dat)),
               gHT= grep("^g_ht", names(dat)),
               BV = grep("^bv", names(dat)),
               gBV = grep("g_bv", names(dat)),
               HTOBS = grep("^HTTCR", names(dat)),
               gHTOBS = grep("g_HTTCR", names(dat)),
               CANHT = grep("^canht", names(dat)),
               gCANHT = grep("g_canht", names(dat)),
               DECM = grep("DECM", names(dat)),
               CPOS = grep("^CPOS", names(dat))),
               v.names = c("BA", "gBA", "STAT", "DBH", "gDBH", "HT", "gHT", "BV", "gBV",
               "HTOBS", "gHTOBS", "CANHT", "gCANHT", "DECM", "CPOS"),
               timevar = "YEAR")
dat$YEAR <- factor(dat$YEAR)
pp <- dat[!is.na(dat$DBH) | !is.na(dat$HT), ]
saveRDS(pp, "temp/pp.rds")

################################################################################
##
##                                    MNM
##
################################################################################
source("../mnm/mnm.R")
source("../mnm/mnm-to-matrix.R")
source('../hood_functions.R')

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

nCols <- c("ba", "id", "bqudx", "bqudy", "spec", "elevcl", "aspcl", "decm", "cpos",
           "gdbh", "ght", "ghtobs", "canht", "gcanht", "gba", "gbv", "bv", "ht",
           "htobs")

dPars <- quote(!is.na(ba) &
               stat == "ALIVE" &
               !is.na(bqudx) &
               bqudx < 11 &
               bqudx > 0 &
               !is.na(bqudy) &
               bqudy < 11 &
               bqudy > 0 &
               pplot > 3)

for (i in 1:3) {
  if (!file.exists(paste0("temp/nm", i, ".rds"))) {
    nLst <- mnm(tPars = tPars, nPars = nPars, dPars = dPars, nCols = nCols,
                nRad = i, dat = matDat, parallel=F)
    nm <- mnm_to_matrix(nLst)
    nm <- addDists(nm)  # add neighbor distances from targets
    saveRDS(nm, paste0("temp/nm", i, ".rds"))
  }
}

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

saveRDS(tp, "temp/tp.rds")

## ## Add canopy heights
## inds <- match(interaction(dat$TPLOT, dat$TRAN, dat$YEAR),
##               interaction(hh_plot$TPLOT, hh_plot$TRAN, hh_plot$time))
## dat$CANHT <- hh_plot[inds, "ht_mean"]
