###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     prep_lisurvey.R
## Overview: prepares open-ended survey responses in the LI survey data for subsequent
##           analyses in analyses_lisurvey.R
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("tidyverse","foreign","car","quanteda",
         "gridExtra","stargazer","xtable","VGAM")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## data directory
datsrc <- "~/Dropbox/Uni/Data/"

## load data
raw <- read.dta(paste0(datsrc,"lisurvey/combined123.dta"))



###########################
### recode time-series data

## respondent id
raw$id <- rownames(raw)
lidat <- data.frame(id = raw$id)

## ideology (factor/dummies)
lidat$ideol <- recode(raw$ideol, "'liberal' = 'Liberal'; 'moderate'='Moderate'
                      ; 'conservative'='Conservative'; else=NA")
lidat$ideol <- factor(lidat$ideol, levels = c("Liberal","Moderate","Conservative"))

## follow politics
lidat$follow <- (4 - recode(as.numeric(raw$follow), "5=NA"))/3

## political activism
lidat$polact <- ((recode(raw$polact1, "'no answer'=NA")=="yes") +
                   (recode(raw$polact2, "'no answer'=NA")=="yes") +
                   (recode(raw$polact3, "'no answer'=NA")=="yes") +
                   (recode(raw$polact4, "'no answer'=NA")=="yes"))/4

## vote in previous election
lidat$vote98 <- recode(raw$vote98, "'yes'=1; 'no'=0; else=NA")

## age
lidat$age <- recode(raw$age, "111=NA")

## sex
lidat$female <- recode(raw$gender, "'female'=1; 'male'=0; else=NA")

## race
lidat$black <- recode(as.numeric(raw$race), "c(2,7)=1; c(9,10)=NA; else=0")

## religiosity (church attendance)
lidat$relig <- (recode(as.numeric(raw$relattnd), "c(7,8)=NA") - 1)/5

## education (bachelor degree)
lidat$educ <- as.numeric(recode(as.numeric(raw$educ), "c(15,16)=NA") >= 11)



##################################
### prepare open-ended survey data

## load dictionary
dict_list <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  read.csv(paste0("in/graham/",x,"_noregex.csv"), stringsAsFactors = F)
})
names(dict_list) <- gsub("\\..*","",names(dict_list))

dict <- sapply(dict_list, paste, collapse = " ")

## regex replacements for dictionary
dict_df <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
  cbind(read.csv(paste0("in/graham/",x,".csv"), allowEscapes = T, stringsAsFactors = F)[[1]]
        , read.csv(paste0("in/graham/",x,"_noregex.csv"), stringsAsFactors = F)[[1]])
}) %>% do.call("rbind", .)

## pre-process open-ended data and calculate mft scores
lisim <- mftScore(opend = select(raw, deslib,deslibb,descon,desconb)
                  , id = raw$id, dict = dict, regex = dict_df, dict_list = dict_list)
lisim_lib <- mftScore(opend = select(raw, deslib,deslibb)
                      , id = raw$id, dict = dict, regex = dict_df, dict_list = dict_list)
lisim_con <- mftScore(opend = select(raw, descon,desconb)
                      , id = raw$id, dict = dict, regex = dict_df, dict_list = dict_list)


### merge survey with mft data

## descriptions of liberals
lidat_lib <- merge(lidat, lisim_lib) %>% mutate(year = "liberals") %>% filter(wc>0)
lidat_lib %>% group_by(ideol) %>% 
  summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

## descriptions of conservatives
lidat_con <- merge(lidat, lisim_con) %>% mutate(year = "conservatives") %>% filter(wc>0)
lidat_con %>% group_by(ideol) %>% 
  summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

## all open-ended responses
lidat <- merge(lidat, lisim) %>% mutate(year = "all responses") %>% filter(wc>0)
lidat %>% group_by(ideol) %>% 
  summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")



#######################################
### save output for analyses_lisurvey.R

save(lidat, lidat_lib, lidat_con, mftLabs, polLabs, file="out/prep_lisurvey.RData")
