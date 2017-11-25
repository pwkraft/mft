###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     MASTER.R
## Overview: installs all required packages, prepares data, and runs analyses
## Author:   Patrick Kraft
###########################################################################################

rm(list=ls())
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## install checkpoint
if(!"checkpoint" %in% installed.packages()){
  install.packages("checkpoint")
}

## reproducible package loading
library(checkpoint)
checkpoint("2017-01-01")


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


