### hood_functions.R --- 
## Filename: hood_functions.R
## Description: Neighborhood functions
## Author: Noah Peart
## Created: Sun Apr 12 18:25:37 2015 (-0400)
## Last-Updated: Fri Apr 17 14:12:27 2015 (-0400)
##           By: Noah Peart
######################################################################
## Possible neighborhood sizes: [9, 25, 49]
source("~/work/functions/functions-coordinates.R")  # euclidean

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

## Add neighborhood distances
addDists <- function(nm, distName="distance") {
    nm[[distName]] <- matrix(nrow=dim(nm[["id"]])[1], ncol=dim(nm[["id"]])[2])
    for (i in seq_along(nm$targets$id)) {
        nebs <- cbind(nm[["bqudx"]][i,], nm[["bqudy"]][i,])
        nebs <- t(nebs[complete.cases(nebs), ])
        if (dim(nebs)[1] == 1) nebs <- t(nebs)
        if (dim(nebs)[2] > 0)
            nm[[distName]][i, 1:dim(nebs)[2]] <- euc(nebs, t(nm$targets[i, c("bqudx", "bqudy")]))
    }
    return ( nm )
}

## library(stats4)
## library(MASS)
## library(bbmle)
## set.seed(1001)
## N <- 100
## x <- rnorm(N, mean=3, sd=2)
## LL <- function(mu, sigma) {
##     -sum(dnorm(x, mu, sigma, log=TRUE))
## }
## (distr <- fitdistr(x, "normal"))$loglik
## summary(est1 <- mle(LL, start=list(mu=1, sigma=1)))
## summary(est2 <- mle2(LL, start=list(mu=1, sigma=1)))

## ## Bounded
## summary(est1 <- mle(LL, start=list(mu=1, sigma=1), method="L-BFGS-B",
##                     lower=c(-Inf, 0), upper=c(Inf, Inf)))
## summary(est2 <- mle2(LL, start=list(mu=1, sigma=1), method="L-BFGS-B",
##                      lower=c(m=-Inf, sigma=0), upper=c(Inf, Inf)))

## ## Linear model
## x <- runif(N)
## y <- 5*x + 3 + rnorm(N)
## summary(fit <- lm(y ~ x))
## -2*logLik(fit)
## plot(x, y)
## abline(fit, col="red", lty=2)

## LL <- function(beta0, beta1, mu, sigma) {
##     R <- y - x*beta1 - beta0
##     R <- suppressWarnings(dnorm(R, mu, sigma))
##     -sum(log(R))
## }

## # summary(fit1 <- mle(LL, start=list(beta0=1, beta1=1, mu=0, sigma=1)))
## summary(fit1 <- mle(LL, start=list(beta0=4, beta1=2, mu=0, sigma=1)))  # matches lm
## summary(fit2 <- mle2(LL, start=list(beta0=4, beta1=2, mu=0, sigma=1)))  # matches lm

## ## Fix mu to 0
## summary(fit3 <- mle(LL, start=list(beta0=4, beta1=2, sigma=1), fixed=list(mu=0)))

## ## Not inverting Hessian
## (summary(fit4 <- mle2(LL, start=list(beta0=4, beta1=2, mu=0, sigma=1))))
## plot(x, y)
## abline(fit, col="red", lty=2)
## xs <- c(min(x), max(x))

## lines(c(min(x), max(x)), c(predict(fit4, newdata = data.frame(x=c(min(x), max(x))))))




## ## DBH distribution
## hist(na.omit(pp$DBH), freq=FALSE)

## ## Exponential
## dDBHexp <- fitdistr(na.omit(pp$DBH), densfun="exponential")
## curve(coef(tst2)*exp(-coef(tst2)*x), add=T)

## ## Weibull
## dDBHweibull <- fitdistr(na.omit(pp[!is.na(pp$DBH)&pp$DBH>0,]$DBH), densfun="weibull")
## k <- coef(dDBHweibull)[["shape"]]
## lambda <- coef(dDBHweibull)[["scale"]]
## curve(k/lambda * (x/k)^(k - 1) * exp(-x/lambda)^k, add=TRUE, from=0, to=60, lty=2, col="green")
## dDBHbinom <- fitdistr(na.omit(pp$DBH), densfun="negative binomial")

