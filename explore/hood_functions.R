### hood_functions.R --- 
## Filename: hood_functions.R
## Description: Neighborhood functions
## Author: Noah Peart
## Created: Sun Apr 12 18:25:37 2015 (-0400)
## Last-Updated: Sun Apr 12 21:02:29 2015 (-0400)
##           By: Noah Peart
######################################################################
## Possible neighborhood sizes: [9, 25, 49]

## Get neighbors of tree
## dist: max distance from target to search for neighbors
findNbrs <- function(id, yr, ndist, dat) {
    targ <- dat[dat$id == id & dat$YEAR == yr, ]
    samp <- dat[dat$PPLOT == targ$PPLOT & dat$YEAR == yr, ]
    inds <- samp$STAT == "ALIVE" & samp$id != id & !is.na(samp$BQUDX) & !is.na(samp$BQUDY) &
        !is.na(samp$DBH) & abs(samp$BQUDX - targ$BQUDX) <= ndist &
            abs(samp$BQUDY - targ$BQUDY) <= ndist
    samp[inds, ]
}

## Get targets given nsize
findTargets <- function(plot, ndist, yr, dat) {
    samp <- dat[dat$PPLOT == plot & dat$YEAR == yr, ]
    inds <- samp$STAT == "ALIVE" & !is.na(samp$BQUDX) & !is.na(samp$BQUDY) &
        !is.na(samp$DBH) & samp$BQUDX >= 1+ndist & samp$BQUDX <= 10-ndist &
            samp$BQUDY >= 1+ndist & samp$BQUDY <= 10-ndist
    samp[inds, ]
}


