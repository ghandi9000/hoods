### power-nci.R --- 
## Filename: power-nci.R
## Description: 
## Author: Noah Peart
## Created: Tue Apr 14 20:24:18 2015 (-0400)
## Last-Updated: Fri Apr 17 23:37:09 2015 (-0400)
##           By: Noah Peart
######################################################################

## cant get line breaks to work when rendered with renderText in shiny
printModel <- function(growthVar, tSizeVar, nSizeVar) {
    res <- paste0("
$$
Size Effect = PG (target_{", toupper(tSizeVar), "})^b
$$
$$
NCI = \\sum_{i}^{N}\\frac{nbr_{", toupper(nSizeVar), "_i}^\\alpha}{distance_{i, target}^\\beta}
$$
$$
Competition Effect = e^{C*NCI^D}
$$
$$
Growth(", growthVar, ") = (Size Effect)(Competition Effect)
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
    PG = ps[["PG"]]
    b = ps[["b"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    C = ps[["C"]]
    D = ps[["D"]]
    
    size.effect = PG * targSize^b
    nci = rowSums(nbrSizes^alpha / nbrDists^beta, na.rm = TRUE)
    competition.effect = exp(C * nci^D)
    size.effect * competition.effect
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
                control=list(maxit = maxit),
                skip.hessian = skip.hessian)
    return( fit )
}

################################################################################
##
##                                   Test
##
################################################################################
## Parameters
## sd > 0, C > 0, PG > 0
## ps <- list(PG=0.003358567, b=1, alpha=1, beta=1, C=1, D=1, sd=1)
## lows <- c(PG=0, alpha=-Inf, beta=-Inf, D=-Inf, C=-Inf, sd=1e-8)
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

