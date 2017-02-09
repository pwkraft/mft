###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     prep_feinberg.R
## Overview: prepare Feinberg data for analyses
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

## load newspaper articles from feinberg
fbrg_text <- read.csv("in/feinberg/environment_articles.csv")

## load feinberg coding
fbrg_mft <- read.spss("in/feinberg/Environment Article Coding.sav"
                      , to.data.frame = T) %>%
  select(NEWSPAPER,ARTICLENUMBER,HarmCare:Purity)

## change id
fbrg_mft$id <- paste(gsub("(^\\s+|\\s+$)","", fbrg_mft$NEWSPAPER)
                     , fbrg_mft$ARTICLENUMBER)

## change variable names
colnames(fbrg_mft) <- c("paper","article","harm","fairness","authority","ingroup","purity","id")

## Note that there might be a mistake in the SPSS dataset (USA Today 22 & 23 switched)
fbrg_mft$id[fbrg_mft$id != fbrg_text$id]

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

## minor pre-processing
fbrg_text$processed <- paste(fbrg_text$title, fbrg_text$text) %>%
  toLower() %>% gsub("(^\\s+|\\s+$)","", .) %>% gsub("//"," ", ., fixed = T) %>%
  gsub("[[:punct:]]"," ", .) %>% gsub("\\n"," ", .) %>% gsub("\\s+"," ", .) %>%
  gsub("(^\\s+|\\s+$)","", .)



## replace regular expressions with word stems
pb <- txtProgressBar(min = 0, max = nrow(dict_df), style = 3)
for(i in 1:nrow(dict_df)){
  fbrg_text$processed <- gsub(dict_df[i,1], dict_df[i,2], fbrg_text$processed)
  setTxtProgressBar(pb, i)
}
close(pb)

## combine dictionary and responses in common dfm
fbrg_dfm <- corpus(c(dict, fbrg_text$processed)
                   , docnames = c(names(dict), as.character(fbrg_text$id))) %>% dfm()
fbrg_dfm <- fbrg_dfm[as.character(fbrg_text$id),]

## meta-info: overall response length
fbrg_text$wc <- apply(fbrg_dfm, 1, sum)

## convert dfm to relative term frequency
fbrg_rtf <- apply(fbrg_dfm, 2, function(x) x/fbrg_text$wc)
fbrg_rtf <- fbrg_rtf[,dict_df[,2]]

## convert dfm to tfidf
fbrg_tfidf <- fbrg_dfm %>% tfidf(normalize=T,k=1)
fbrg_tfidf <- fbrg_tfidf[,dict_df[,2]]

## count relative term frequencies & tfidf weights for each media source
sim <- data.frame(
  authority_rtf = apply(fbrg_rtf[,dict_list$authority],1,sum)
  , fairness_rtf = apply(fbrg_rtf[,dict_list$fairness],1,sum)
  , harm_rtf = apply(fbrg_rtf[,dict_list$harm],1,sum)
  , ingroup_rtf = apply(fbrg_rtf[,dict_list$ingroup],1,sum)
  , purity_rtf = apply(fbrg_rtf[,dict_list$purity],1,sum)
  , authority_tfidf = apply(fbrg_tfidf[,dict_list$authority],1,sum)
  , fairness_tfidf = apply(fbrg_tfidf[,dict_list$fairness],1,sum)
  , harm_tfidf = apply(fbrg_tfidf[,dict_list$harm],1,sum)
  , ingroup_tfidf = apply(fbrg_tfidf[,dict_list$ingroup],1,sum)
  , purity_tfidf = apply(fbrg_tfidf[,dict_list$purity],1,sum)
)
sim$id <- rownames(sim)

## combine similarity results
fbrg_mft <- merge(fbrg_mft, sim)



################
### save results

save(fbrg_mft, fbrg_text, file="out/prep_fbrg.RData")
