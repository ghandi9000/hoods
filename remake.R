### remake.R --- 
## Filename: remake.R
## Description: Remake datasets for analysis
## Author: Noah Peart
## Created: Mon Apr 13 20:07:47 2015 (-0400)
## Last-Updated: Mon Aug  3 14:12:06 2015 (-0400)
##           By: Noah Peart
######################################################################
source("./helpers.R", chdir=T)
require(plyr)
require(dplyr)

## Data with estimated heights/boles
tryCatch({
    pp <- read.csv("../treedata/pp.csv")
    tp <- read.csv("../treedata/transect.csv")
}, error=function(e) 
    stop("\n\n*** No data found -- run the update to get the data ***\n\n"))

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
               BA = grepInOrder("^BA", yrs, dat),
               gBA = grepInOrder("g_BA", yrs, dat),
               STAT = grepInOrder("^STAT", yrs, dat),
               DBH = grepInOrder("^DBH", yrs, dat),
               gDBH = grepInOrder("^g_DBH", yrs, dat),
               HT = grepInOrder("^ht", yrs, dat),
               gHT= grepInOrder("^g_ht", yrs, dat),
               BV = grepInOrder("^bv", yrs, dat),
               gBV = grepInOrder("g_bv", yrs, dat),
               HTOBS = grepInOrder("^HTTCR", yrs, dat),
               gHTOBS = grepInOrder("g_HTTCR", yrs, dat),
               CANHT = grepInOrder("^canht", yrs, dat),
               gCANHT = grepInOrder("g_canht", yrs, dat),
               DECM = grepInOrder("DECM", yrs, dat),
               CPOS = grepInOrder("^CPOS", yrs, dat)),
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
source("mnm/mnm.R")
source("mnm/mnm-to-matrix.R")
source('hood_functions.R')

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
cols <- grep("canht|^STAT|^DBH|^HT[[:digit:]]|^ht[[:digit:]]+|^bv|TRAN|TPLOT|TAG|SPEC|ASP|ELEV|DIST|^HR$|TRAD|ABSRAD", names(tp))
dat <- tp[, cols]
cols <- grep("[A-Za-z]+$|87$|98$|99$|10$|11$", names(dat))
dat <- dat[, cols]  # remove other year columns
dat[, paste0("HT", c(87, 98, 10))] <- rep(NA, nrow(dat))
dat <- rename(dat, ABSRAD99=ABSRAD, TRAD99=TRAD)
dat[, paste0("ABSRAD", c(87, 98, 10))] <- NA
dat[, paste0("TRAD", c(87, 98, 10))] <- NA

## Growth columns
## 87, 98 => no S transects, no HH of LL elevations
dat[, paste0("BA",yrs)] <- 0.00007854 * dat[,paste0("DBH", yrs)]**2
## vars <- c("DBH", "ht", "bv", "canht", "HTTCR", "BA")
## for (v in vars) {
##     dat[,paste0("g_", v, 86)] <- (dat[,paste0(v, 98)] - dat[,paste0(v, 86)])/12
##     dat[,paste0("g_", v, 87)] <- (dat[,paste0(v, 98)] - dat[,paste0(v, 87)])/11
##     dat[,paste0("g_", v, 98)] <- (dat[,paste0(v, 10)] - dat[,paste0(v, 98)])/12
## }

dat <- reshape(dat, times = yrs, direction = "long",
               varying = list(
               TRAD = grepInOrder("^TRAD", yrs, dat),
               ABSRAD = grepInOrder("^ABSRAD", yrs, dat),
               STAT = grepInOrder("^STAT", yrs, dat),
               DBH = grepInOrder("^DBH", yrs, dat),
               BV = grepInOrder("^bv", yrs, dat),
               HTOBS = grepInOrder("^HT", yrs, dat),
               HT = grepInOrder("^ht", yrs, dat),
               CANHT = grepInOrder("canht", yrs, dat)),
               v.names = c("TRAD", "ABSRAD", "STAT", "DBH", "BV", "HTOBS", "HT", "CANHT"),
               timevar = "YEAR")
dat$YEAR <- factor(dat$YEAR)

tp <- dat[!is.na(dat$DBH) | !is.na(dat$HT) | !is.na(dat$HTOBS), ]
tp$BA <- 0.00007854*tp$DBH*tp$DBH

## Polar -> cartesian
coords <- pol2cart(tp$DIST, (tp$HR%%12)/12 * 2*pi + pi/2)
tp$X <- -coords[,1]
tp$Y <- coords[,2]

## Save
saveRDS(tp, "temp/tp.rds")
