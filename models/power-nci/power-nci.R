### power-nci.R --- 
## Filename: power-nci.R
## Description: 
## Author: Noah Peart
## Created: Tue Apr 14 20:24:18 2015 (-0400)
## Last-Updated: Tue Apr 14 21:44:16 2015 (-0400)
##           By: Noah Peart
######################################################################

## cant get line breaks to work when rendered with renderText in shiny
printModel <- function(sizeVar) {
    res <- paste0("
$$
Size Effect = PG (target", toupper(sizeVar), ")^b
$$
$$
NCI = \\sum_{i}^{N}\\frac{", toupper(sizeVar), "^\\alpha}{dist_{i}^\\beta}
$$
$$
Competition Effect = e^{-C*NCI^D}
$$
$$
Growth = (Size Effect)(Competition Effect)
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
    D = ps[["D"]]
    
    size.effect = PG * targSize^b
    nci = rowSums(nbrSizes^alpha / nbrDists^beta, na.rm = TRUE)
    competition.effect = nci^D
    size.effect * competition.effect
}
