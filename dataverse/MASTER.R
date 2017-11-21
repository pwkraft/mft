###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     MASTER.R
## Overview: Installs required packages and executes all scripts to prepare data,
##           estimate models, and present results
## Author:   Patrick Kraft
###########################################################################################

## clear workspace
rm(list=ls())

## install required packages
pkg <- c("tidyverse","quanteda","xtable","gridExtra"
         ,"readstata13","car","VGAM","MASS", "sandwich")
inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
rm(pkg,inst)

## create folders to save output data, figures, and tables
dir.create("out", showWarnings = FALSE)
dir.create("fig", showWarnings = FALSE)
dir.create("tab", showWarnings = FALSE)


### Main analyses (2012 ANES)

## prepare dataset
source("anes_prep.R", echo=T, max.deparse.length=10000)

## run main analyses
source("anes_analyses.R", echo=T, max.deparse.length=10000)


### Supplementary analyses for appendix

## 2012 ANES appendix
source("anes_appendix.R", echo=T, max.deparse.length=10000)

## LI survey analyses
source("app_lisurvey.R", echo=T, max.deparse.length=10000)

## Feinberg replication
source("app_feinberg.R", echo=T, max.deparse.length=10000)
