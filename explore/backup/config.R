#/usr/bin/env Rscript
### config.R --- 
## Filename: config.R
## Description: 
## Author: Noah Peart
## Created: Tue Apr 14 12:05:33 2015 (-0400)
## Last-Updated: Tue Apr 14 12:33:46 2015 (-0400)
##           By: Noah Peart
######################################################################
cat("-----------------------------------------------------\n")
cat("\n\n\t\tChecking/installing packages...\n\n")
cat("-----------------------------------------------------\n")
packages <- c("dplyr", "plyr", "xtable", "shiny", "magrittr",
              "lazyeval", "ggplot2", "grid")  # required packages
for (pack in packages) {
    cat(paste("-->", pack, "\n"))
    if (pack %in% rownames(installed.packages()) == FALSE)
        install.packages(pack)
}

cat("-----------------------------------------------------\n")
cat("\n\t\tRunning setup\n\n")
cat("-----------------------------------------------------\n")
source("setup.R")
cat("\n\n\t\tDone...\n\n")
