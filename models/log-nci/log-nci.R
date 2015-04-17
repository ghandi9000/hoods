### log-nci.R --- 
## Filename: log-nci.R
## Description: Size logistic nci model
## Author: Noah Peart
## Created: Tue Apr 14 17:40:40 2015 (-0400)
## Last-Updated: Fri Apr 17 12:32:23 2015 (-0400)
##           By: Noah Peart
######################################################################

## cant get line breaks to work when rendered with renderText in shiny
printModel <- function(sizeVar) {
    res <- paste0("
$$
Size Effect = e^{-\\frac{log(\\frac{target", toupper(sizeVar), "}{X_0})}{2 X_b}^2}
$$
$$
NCI = \\sum_{i}^{N}\\frac{", toupper(sizeVar), "^\\alpha}{dist_{i}^\\beta}
$$
$$
Competition Effect = e^{-C*NCI^D}
$$
$$
Growth = (PG)(Size Effect)(Competition Effect)
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
    Xb <- ps[["Xb"]]
    
    sizeEffect <- exp(-0.5 * ( log(targSize/X0) / Xb)^2 )
    nci <- rowSums(nbrSizes^alpha / nbrDists^beta, na.rm=TRUE)
    compEffect <- exp(-C * nci^D)
    PG * sizeEffect * compEffect
}

## Run model
run_fit <- function(nms, growthVar, tSizeVar, nSizeVar, ps, yr, method="L-BFGS-B", maxit=1e5) {
    require(bbmle)
    parnames(normNLL) <- c(names(ps))
    ht <- paste0("HT", yr)
    dbh <- paste0("DBH", yr)
    fit <- mle2(normNLL,
                start = unlist(ps, recursive = FALSE),
                data = list(
                x=nms$targets[[growthVar]],
                targSize=nms$targets[[sizeVar]],
                nbrSizes=nms[[nSizeVar]],
                nbrsDists=nms[["distance"]]),
                method = method,
                control = list(maxit = maxit))
    return( fit )
}

################################################################################
##
##                                   Test
##
################################################################################
## Parameters
## X0 > 0, sd > 0, Xb > 0, C > 0, PG > 0
ps <- list(PG=0.003358567, alpha=1, beta=1, X0=1, Xb=1, C=1, D=1, sd=1)
lows <- c(0, -Inf, -Inf, 0, 0, 0, -Inf, 0)
highs <- c(Inf, Inf, Inf, Inf, Inf, Inf, Inf)

## nm <- readRDS("temp/nm2.rds")
