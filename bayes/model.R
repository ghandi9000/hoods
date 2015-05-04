### model.R --- 
## Filename: model.R
## Description: 
## Author: Noah Peart
## Created: Sun May  3 17:33:51 2015 (-0400)
## Last-Updated: Sun May  3 22:39:43 2015 (-0400)
##           By: Noah Peart
######################################################################

dat <- read.csv("p4.csv")
mod <- nls(gBA ~ a * BA**b, start=list(a=1, b=.3), data=dat)

with(dat, plot(BA, gBA, col="blue"))
points(dat$BA, predict(mod), col="red")

fit <- lm(dat$gBA ~ dat$BA)
