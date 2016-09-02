###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     master.R
## Overview: installs all required packages, prepares data, and runs analyses
## Author:   Patrick Kraft
###########################################################################################

rm(list=ls())
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load packages
pkg <- c("reshape2","ggplot2","readstata13","car","dplyr","quanteda","stargazer"
         ,"xtable","VGAM","gridExtra","devtools")
inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
invisible(lapply(pkg, library, character.only = TRUE))
if(!"pmisc" %in% installed.packages()) install_github("pwkraft/pmisc")
invisible(library(pmisc))
rm(pkg,inst)

## prepare dataset
source("prep.R")

## run main analyses
source("analyses.R")

## run analyses for appendix
source("appendix.R")
