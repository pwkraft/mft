###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     MASTER.R
## Overview: installs all required packages, prepares data, and runs analyses
## Author:   Patrick Kraft
###########################################################################################

rm(list=ls())
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")


### Install required packages

pkg <- c("tidyverse","readstata13","car","quanteda","stargazer"
         ,"xtable","VGAM","gridExtra","MASS", "sandwich")
inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
rm(pkg,inst)


### ANES analyses

## prepare dataset
source("prep_anes.R", echo=T, max.deparse.length=10000)

## run main analyses
source("analyses_anes.R", echo=T, max.deparse.length=10000)

## run analyses for appendix
source("appendix_anes.R", echo=T, max.deparse.length=10000)


### LI survey analyses

## prepare dataset
source("prep_lisurvey.R", echo=T, max.deparse.length=10000)

## run main analyses
source("analyses_lisurvey.R", echo=T, max.deparse.length=10000)

## run analyses for appendix
source("appendix_lisurvey.R", echo=T, max.deparse.length=10000)


### Feinberg replication

## prepare dataset
source("prep_feinberg.R", echo=T, max.deparse.length=10000)

## run main analyses
source("analyses_feinberg.R", echo=T, max.deparse.length=10000)

## run analyses for appendix
source("appendix_feinberg.R", echo=T, max.deparse.length=10000)


