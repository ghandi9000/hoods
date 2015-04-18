### log-nci.R --- 
## Filename: log-nci.R
## Description: Size logistic nci model
## Author: Noah Peart
## Created: Tue Apr 14 17:40:40 2015 (-0400)
## Last-Updated: Fri Apr 17 23:34:00 2015 (-0400)
##           By: Noah Peart
######################################################################

## cant get line breaks to work when rendered with renderText in shiny
printModel <- function(growthVar, tSizeVar, nSizeVar) {
    res <- paste0("
$$
Size Effect = e^{-\\frac{log(\\frac{target", toupper(tSizeVar), "}{X_0})}{2 X_b}^2}
$$
$$
NCI = \\sum_{i}^{N}\\frac{", toupper(nSizeVar), "^\\alpha}{dist_{i}^\\beta}
$$
$$
Competition Effect = e^{-C*NCI^D}
$$
$$
Growth(", growthVar, ") = (PG)(Size Effect)(Competition Effect)
$$

Where 'N' is the number of neighbors of a given target.")
    
    return( res )
}

## log-likelhood
normNLL <- function(params, x, targSize, nbrSizes, nbrDists) {
    sd <- params[["sd"]]
    mu <- do.call(model, list(params, targSize, nbrSizes, nbrDists))
    -sum(dnorm(x, mean=mu, sd=sd, log=TRUE))
}

## nbrSizes and nbrDists are matrices, each row contains neighbors for a single target
## targSize is vector of target sizes
model <- function(ps, targSize, nbrSizes, nbrDists) {
    PG <- ps[["PG"]]
#    b <- ps[["b"]]
    alpha <- ps[["alpha"]]
    beta <- ps[["beta"]]
    D <- ps[["D"]]
    C <- ps[["C"]]
    X0 <- ps[["X0"]]
    ## Xb <- ps[["Xb"]]
    
    sizeEffect <- exp(-0.5 * ( log(targSize/X0) )^2 )
    nci <- rowSums(nbrSizes^alpha / nbrDists^beta, na.rm=TRUE)
    compEffect <- exp(-C * nci^D)
    PG * sizeEffect * compEffect
}

## Run model
run_fit <- function(nms, growthVar, tSizeVar, nSizeVar, ps,
                    method="L-BFGS-B", maxit=1e5, skip.hessian=T, ...) {
    require(bbmle)
    parnames(normNLL) <- c(names(ps))
    targs <- !is.na(nms$targets[[gVar]])
    fit <- mle2(normNLL,
                start = unlist(ps, recursive = FALSE),
                data = list(
                x=nms$targets[targs, growthVar],
                targSize=nms$targets[targs, sizeVar],
                nbrSizes=nms[[nSizeVar]][targs,],
                nbrDists=nms[["distance"]][targs,]),
                method=method,
                lower=lows,
                upper=highs,
                control=list(maxit = maxit), skip.hessian = skip.hessian)
    return( fit )
}

################################################################################
##
##                                   Test
##
################################################################################
## Parameters
## X0 > 0, sd > 0, Xb > 0, C > 0, PG > 0
## ps <- list(PG=0.003358567, alpha=1, beta=1, X0=1, C=1, D=1, sd=1)
## lows <- c(PG=0, alpha=-Inf, beta=-Inf, X0=1e-8, C=0, D=-Inf, sd=1e-8)
## highs <- rep(Inf, length(lows))
## names(highs) <- names(lows)
## highs[["sd"]] <- 2
## gVar <- "gba"
## tSize <- "ba"
## nSize <- "ba"

## ## data, only targets with growth value
## targs <- !is.na(nm$targets[[gVar]])

## ## nm <- readRDS("temp/nm2.rds")
## tst <- model(ps, targSize = nm$targets[targs,]$ba,
##              nbrSizes = nm$ba[targs,],
##              nbrDists = nm$distance[targs,])

## fit <- run_fit(nms=nm, growthVar=gVar, tSizeVar=tSize, nSizeVar=nSize, ps=ps,
##                skip.hessian=TRUE, maxit=1e7, method="SANN")

## nps <- unlist(coef(fit))
## fit <- run_fit(nms=nm, growthVar=gVar, tSizeVar=tSize, nSizeVar=nSize, ps=ps,
##                lower=lows, upper=highs)

