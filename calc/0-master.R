###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     master.R
## Overview: installs all required packages, prepares data, and runs analyses
## Author:   Patrick Kraft
###########################################################################################

rm(list=ls())
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load packages
pkg <- c("reshape2","ggplot2","Hmisc","MASS","sandwich")
pkg_github <- c("pmisc")
inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(pkg,inst)
