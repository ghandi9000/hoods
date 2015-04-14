### log-nci.R --- 
## Filename: log-nci.R
## Description: Size logistic nci model
## Author: Noah Peart
## Created: Tue Apr 14 17:40:40 2015 (-0400)
## Last-Updated: Tue Apr 14 18:47:20 2015 (-0400)
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
Competition Effect = e^{-C NCI^D}
$$
$$
Growth = (PG)(Size Effect)(Competition Effect)
$$
Where $N$ is the number of neighbors of a given target.")
    
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
    b <- ps[["b"]]
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
