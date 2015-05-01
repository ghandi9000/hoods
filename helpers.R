### helpers.R --- 
## Filename: helpers.R
## Description: 
## Author: Noah Peart
## Created: Fri May  1 12:42:09 2015 (-0400)
## Last-Updated: Fri May  1 12:42:39 2015 (-0400)
##           By: Noah Peart
######################################################################

## grep indices ordered by year
grepInOrder <- function(coln, yrs, dat) {
    unlist(sapply(paste0(coln, yrs), function(x) grep(x, names(dat))))
}

## Polar to cartesian
## deg: if input theta is in degrees
pol2cart <- function(r, theta, deg = FALSE, recycle = FALSE) {
    if (deg) theta <- theta * pi/180
    if (length(r) > 1 && length(r) != length(theta) && !recycle)
        stop("'r' vector different length than theta, if recycling 'r' values is desired 'recycle' must be TRUE")
    xx <- r * cos(theta)
    yy <- r * sin(theta)
    ## Account for machine error in trig functions
    xx[abs(xx) < 2e-15] <- 0
    yy[abs(yy) < 2e-15] <- 0
    out <- cbind(xx, yy)
    colnames(out) <- c("x", "y")
    return( out )
}

## circle
circ <- function(radii, yrs) {
    rads <- seq(0, 2*pi, length=40)
    res <- do.call(rbind, lapply(radii, function(r)
        data.frame(X=r*cos(rads), Y=r*sin(rads))))
    res$YEAR <- factor(rep(yrs, each=40))
    return ( res )
}

