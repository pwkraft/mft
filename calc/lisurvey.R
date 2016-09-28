###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     lisurvey.R
## Overview: preliminary analyses of LI survey data
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("foreign","car","dplyr","quanteda")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## data directory
datsrc <- "~/Dropbox/Uni/Data/"

## load data
lidat <- read.dta(paste0(datsrc,"lisurvey/combined123.dta"))
lidat$id <- rownames(lidat)

## load dictionary
dict <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  read.csv(paste0("in/graham/",x,"_noregex.csv")) %>%
    sapply(paste, collapse = " ") %>% as.character()
})

## regex replacements for dictionary
dict_df <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  cbind(read.csv(paste0("in/graham/",x,".csv"), allowEscapes = T, stringsAsFactors = F)[[1]]
        , read.csv(paste0("in/graham/",x,"_noregex.csv"), stringsAsFactors = F)[[1]])
}) %>% do.call("rbind", .)

## pre-process open-ended data and calculate similarity
lisim <- mftSimilarity(opend = select(lidat, deslib,deslibb,descon,desconb)
                       , id = lidat$id, dict = dict, regex = dict_df)
lisim_lib <- mftSimilarity(opend = select(lidat, deslib,deslibb)
                           , id = lidat$id, dict = dict, regex = dict_df)
lisim_con <- mftSimilarity(opend = select(lidat, descon,desconb)
                       , id = lidat$id, dict = dict, regex = dict_df)

apply(lisim_lib[-2],2,function(x) mean(as.numeric(x))) - apply(lisim_con[-2],2,function(x) mean(as.numeric(x)))

dat <- merge(lidat, lisim)
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

dat <- merge(lidat, lisim_lib)
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

dat <- merge(lidat, lisim_con)
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")
