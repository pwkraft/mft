###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     mft_analyses.R
## Overview: alternative analysis focusing on cosine similarity b/w dictionaries and responses
##           uses the datasets generated in mft_prep
## Author:   Patrick Kraft
###############################################################################################


rm(list=ls())
setwd("/data/Uni/projects/2014/mft/calc")

## load packages
pkg <- c("ggplot2","stargazer","xtable","quanteda","systemfit","dplyr")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(list=ls())

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## load additional functions
source("func/anes_plot.R")

## load recoded dataset
load("out/anes_full.RData")


### calculate cosine similarity between dictionaries and documents

## load responses, create dfm and tfidf
anes2012resp <- apply(anes2012opend$spell[,-1], 1, paste, collapse = " ")
anes2012resp <- gsub("NA\\s*","",anes2012resp)
names(anes2012resp) <- anes2012opend$spell$id
anes2012resp <- anes2012resp[anes2012resp != ""]
#anes2012resp <- corpus(anes2012resp, docnames = names(anes2012resp))
anes_dfm <- dfm(anes2012resp, ignoredFeatures = stopwords("english"), stem = TRUE)
anes_tfidf <- tfidf(anes_dfm)
anes_tfidf@Dimnames$features[1:10]
as.matrix(anes_dfm)[1:5,1:5]
as.matrix(anes_tfidf)[1:5,1:5]

## load dictionary, create dfm and tfidf
dict <- strsplit(paste(read.csv("in/dictionary_noregex.csv")[,1], collapse = " ")
               , "\\s\\w*\\.\\w*\\s")[[1]]
dict <- c(paste(dict[1:2], collapse = " "), paste(dict[3:4], collapse = " "),
          paste(dict[5:6], collapse = " "), paste(dict[7:8], collapse = " "), 
          paste(dict[9:10], collapse = " "), paste(dict[11:12], collapse = " "))
#dict <- corpus(dict, docnames = c("harm","fariness","ingroup","authority","purity","general"))
dict_dfm <- dfm(dict, stem = TRUE)
dict_tfidf <- tfidf(dict_dfm)
dict_dfm@Dimnames$features
as.matrix(dict_dfm)
as.matrix(dict_tfidf)

## create list of 100 most common words that are not in the dictionary
top <- names(topfeatures(anes_dfm, 1000))
top <- paste(top[!top %in% dict_dfm@Dimnames$features][1:500], collapse = " ")

## combine dictionary and responses in common dfm/tfidf
comb <- corpus(c(dict[-6],top,anes2012resp)
             , docnames = c("harm","fairness","ingroup","authority","purity","general"
                           , names(anes2012resp)))
comb_dfm <- dfm(comb, stem = TRUE)
comb_tfidf <- tfidf(comb_dfm)
comb_dfm@Dimnames$docs[1:10]

as.matrix(comb_dfm)[1:5,1:5]
as.matrix(comb_tfidf)[1:5,1:5]

## calculate similarity
#pr_DB$get_entries()
similarity_dfm <- similarity(comb_dfm,
                           , c("harm","fairness","ingroup","authority","purity","general")
                           , margin = "documents", method = "cosine")
similarity_tfidf <- similarity(comb_tfidf
                             , c("harm","fairness","ingroup","authority","purity","general")
                             , margin = "documents", method = "cosine")

lapply(as.matrix(comb_dfm),2,table)
View(as.matrix(comb_dfm))

hist(similarity_tfidf$harm)
hist(similarity_dfm$harm)
sum(similarity_tfidf$harm, na.rm = T)
plot(similarity_tfidf$harm ~ similarity_dfm$harm)
hist(similarity_tfidf$general)

class(similarity_tfidf)
length(similarity_tfidf)
names(similarity_tfidf)

head(comb_dfm@Dimnames$docs)


test <- data.frame(t(as.matrix(similarity_tfidf)))
test$id <- as.numeric(rownames(test))
test <- test %>% arrange(id) %>% filter(!is.na(id))

head(test)
table(apply(test[,-ncol(test)],1,sum)==0)
test$id[which(apply(test[,-ncol(test)],1,sum)==0)] ## these are cases with very short responses, will be omitted for now

fct_norm <- function(x){
    x <- x + 0.0001
    out <- x / sum(x)
    out
}

test_norm <- data.frame(t(apply(test[,-ncol(test)], 1, fct_norm)))
test_norm$id <- test$id
test_norm <- na.omit(test_norm)

table(apply(test_norm[,-ncol(test_norm)],1,sum))
head(test_norm)

test_data <- merge(anes2012merge$data, test_norm)

names(test_data)


plot(harm ~ harm_all, data = test_data)
## there's a problem, similarity is maybe increased if other categories are NOT mentioned...
## I think I have to look at each query independently in order to calculate the distance,
## then combine it in a matrix


## model 1: seemingly unrelated regression on individual scores
eqSystem <- list(harm = harm ~ ideol + relig + educ + age + female + black + num_total
               , fairness ~ ideol + relig + educ + age + female + black + num_total
               , authority ~ ideol + relig + educ + age + female + black + num_total
               , ingroup ~ ideol + relig + educ + age + female + black + num_total
               , purity ~ ideol + relig + educ + age + female + black + num_total)
m1 <- systemfit(eqSystem, "SUR", data = test_data)
## but, this is problematic for multiple reasons...

## model 2: dirichlet model
m2raw <- test_data %>% dplyr::select(harm, fairness, authority, ingroup, purity, general
                                   , ideol_lib, ideol_con, relig, educ, age, female, black
                                   , num_total) %>% na.omit()
m2dl <- list(Y = m2raw[,c("harm", "fairness", "authority", "ingroup", "purity", "general")]
           , X = m2raw[,c("ideol_lib", "ideol_con", "relig", "educ", "age", "female", "black"
                        , "num_total")], I = nrow(m2raw), J = 8, K = 6
           , X_new = data.frame(ideol_lib = c(1,0), idel_con = c(0,1), relig = mean(m2raw$relig)
                              , educ = mean(m2raw$educ), age = mean(m2raw$educ)
                              , female = mean(m2raw$female), black = mean(m2raw$black)
                              , num_total = mean(m2raw$num_total)))
m2stan <- stan(file = "func/mft_dirichlet.stan", data = m2dl)
             

