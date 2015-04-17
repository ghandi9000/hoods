### fit.R --- 
## Filename: fit.R
## Description: Fit neighborhood models
## Author: Noah Peart
## Created: Fri Apr 17 11:45:11 2015 (-0400)
## Last-Updated: Fri Apr 17 11:51:20 2015 (-0400)
##           By: Noah Peart
######################################################################

## Source in one of model scripts, which will contain
## 'model' and 'normNLL' functions

run_fit <- function(dat, ps, yr, method="L-BFGS-B", maxit=1e5) {
    require(bbmle)
    parnames(normNLL) <- c(names(ps))
    ht <- paste0("HT", yr)
    dbh <- paste0("DBH", yr)
    fit <- mle2(normNLL,
                start = unlist(ps, recursive = FALSE),
                data = list(x = dat[, ht], dbh=dat[, dbh], elev=dat[, "ELEV"],
                canht=dat[,"canht"]),
                method = method,
                control = list(maxit = maxit))
    return( fit )
}


### Automated fitting of neighborhood models by MLE
fit.MLE.models <- function(dat, sr, spec, ind.var, dep.var, models=NULL, bigger=TRUE,
                           method="Nelder-Mead", maxit=1000, savefits="currentfits.Rda",
                           realdist = FALSE) {
    srt <- max(sr) # if multiple sr, targets are those in all neighborhoods
    fits <- c()
    if(realdist == FALSE) {
        neighbors <<- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                             bqudy > 0 & stat=="ALIVE")
        targets <<- subset(dat, bqudx < (12-srt) & bqudx > (-1 + srt) &
                           bqudy < (12 - srt) & bqudy > (-1 + srt) & stat=="ALIVE")
    }
    if(realdist == TRUE) {
        targets <<- subset(dat, abs(x) < (11-sr) & abs(y) < (11-sr) & stat=="ALIVE")
        neighbors <<- subset(dat, abs(x) <= 11 & abs(y) <= 11 & stat=="ALIVE")
    }
                                        # remove trees that dont satisfy certain conditions
    grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0 &
                  targets[,ind.var]>0)
    targets <<- targets[grew,]
    for(i in sr) {  # make neighbor matrices
        print(paste("Making neighbor matrices for *", i, "* sized neighborhoods..."))
        make.neighbor.matrices(targets, neighbors, i, ind.var=ind.var, bigger=bigger,
                               realdist = realdist)
                                        # assign matrices in global for later access
        assign(paste0("species",i), species, envir = .GlobalEnv)
        assign(paste0("bas",i), bas, envir = .GlobalEnv)
        assign(paste0("distances",i), distances, envir = .GlobalEnv)
                                        # fit models
        if(!missing(models)) {
            print(paste("Fitting models with sr =", i))
            fits1 <- sapply(models, FUN=function(d) {
                print(paste("Model:", d))
                currentmodel <<- d
                ps <- get.params(sr = i, spec, ind.var, dep.var, d)
                print("Starting Parameters:"); print(unlist(ps,recursive = FALSE))
                parnames(normNLL) <<- c(names(ps))
                fit2 <- mle2(normNLL,
                             start = unlist(ps,recursive = FALSE),
                             data = list(x = targets[,dep.var]),
                             method = method,
                             control = list(maxit = maxit))
                add.params(sr=i, spec, ind.var, dep.var, newpars = coef(fit2), d)
                                        # add fit to current fits saved file
                tmp.env <- new.env() # environment to save fits in
                load(savefits, envir = tmp.env)
                assign(paste(d,sr,spec,sep = "."),fit2,envir=tmp.env)
                save(list=ls(all.names=TRUE, pos=tmp.env),
                     envir=tmp.env, file=savefits)
                rm(tmp.env)
                fit2
            })
            names(fits1) <- paste0(models,i)
            fits <- c(fits, fits1)
        }
    }
    fits
}
