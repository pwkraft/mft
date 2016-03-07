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

## load dictionary
dict <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
    read.csv(paste0("in/graham/",x,"_noregex.csv")) %>%
        sapply(paste, collapse = " ") %>% as.character()
})

dict_df <- sapply(c("authority","fairness","harm","ingroup","purity"), function(x){
    cbind(read.csv(paste0("in/graham/",x,".csv"), allowEscapes = T, stringsAsFactors = F)[[1]]
        , read.csv(paste0("in/graham/",x,"_noregex.csv"), stringsAsFactors = F)[[1]])
}) %>% do.call("rbind", .)

## load alternative dictionary, create dfm and tfidf
dict_new <- strsplit(paste(read.csv("in/dictionary_noregex.csv")[,1], collapse = " ")
                   , "\\s\\w*\\.\\w*\\s")[[1]]
dict_new <- c(paste(dict_new[1:2], collapse = " "), paste(dict_new[3:4], collapse = " "),
          paste(dict_new[5:6], collapse = " "), paste(dict_new[7:8], collapse = " "), 
          paste(dict_new[9:10], collapse = " "), paste(dict_new[11:12], collapse = " "))

## load responses, create dfm and tfidf
anes2012resp <- apply(anes2012opend$spell[,-1], 1, paste, collapse = " ")
anes2012resp <- gsub("NA\\s*","",anes2012resp)
names(anes2012resp) <- anes2012opend$spell$id
anes2012resp <- anes2012resp[anes2012resp != ""]
for(i in 1:nrow(dict_df)){
    anes2012resp <- gsub(dict_df[i,1],dict_df[i,2],anes2012resp)
}

## combine dictionary and responses in common dfm/tfidf
comb <- corpus(c(dict,anes2012resp)
             , docnames = c("authority","fairness","harm","ingroup","purity"
                           , names(anes2012resp)))
comb_dfm <- dfm(comb)
comb_tfidf <- tfidf(comb_dfm)
comb_dfm@Dimnames$docs[1:10]
comb_dfm@Dimnames$features[1:10]

## calculate similarity (check pr_DB$get_entries() for options)



##
##
## try normalized tfidf, think about which one makes more sense...
##
##

similarity_dfm <- similarity(comb_dfm,
                           , selection = c("harm","fairness","ingroup","authority","purity")
                           , margin = "documents", method = "cosine")
similarity_dfm <- data.frame(t(as.matrix(similarity_dfm)))
similarity_dfm$general <- apply(similarity_dfm,1,sum)
similarity_dfm$id <- as.numeric(rownames(similarity_dfm))
similarity_dfm <- similarity_dfm %>% arrange(id) %>% filter(!is.na(id))
data_dfm <- merge(anes2012merge$data, similarity_dfm)
similarity_tfidf <- similarity(comb_tfidf
                             , selection =  c("harm","fairness","ingroup","authority","purity")
                             , margin = "documents", method = "cosine")
similarity_tfidf <- data.frame(t(as.matrix(similarity_tfidf)))
similarity_tfidf$general <- apply(similarity_tfidf,1,sum)
similarity_tfidf$id <- as.numeric(rownames(similarity_tfidf))
similarity_tfidf <- similarity_tfidf %>% arrange(id) %>% filter(!is.na(id))
data_tfidf <- merge(anes2012merge$data, similarity_tfidf)

plot(harm ~ harm_all, data = data_dfm)
## there's a problem, similarity is maybe increased if other categories are NOT mentioned...
## I think I have to look at each query independently in order to calculate the distance,
## then combine it in a matrix


### Preliminary models: SUR
## probllematic because of censoring

## global labels for plots
mftLabs <- c("Authority / \nRespect", "Ingroup / \nLoyalty"
           , "Fairness / \nReciprocity", "Harm / \nCare")
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")
covLabs <- c("Church Attendance","Education (College Degree)","Age","Sex (Female)"
            ,"Race (African American)","Number of Words")


## model 1: mft and indeology
eqSystem <- list(harm = harm ~ ideol + relig + educ + age + female + black + num_total
               , fairness = fairness ~ ideol + relig + educ + age + female + black + num_total
               , authority = authority ~ ideol + relig + educ + age + female + black + num_total
               , ingroup = ingroup ~ ideol + relig + educ + age + female + black + num_total)
m1a <- systemfit(eqSystem, "SUR", data = data_dfm)
m1b <- systemfit(eqSystem, "SUR", data = data_tfidf)
m1res <- rbind(summary(m1a)$coefficients[,c("Estimate","Std. Error")] %>% data.frame() %>%
                    cbind(matrix(unlist(strsplit(rownames(summary(m1a)$coefficients),"\\_"))
                              [unlist(strsplit(rownames(summary(m1a)$coefficients),"\\_"))!="total"]
                            , ncol = 2, byrow = T))
              , summary(m1b)$coefficients[,c("Estimate","Std. Error")] %>% data.frame() %>%
                    cbind(matrix(unlist(strsplit(rownames(summary(m1b)$coefficients),"\\_"))
                              [unlist(strsplit(rownames(summary(m1b)$coefficients),"\\_"))!="total"]
                            , ncol = 2, byrow = T)))
m1res$model <- rep(c("dfm","tfidf"), each = nrow(m1res)/2)
colnames(m1res) <- c("mean","sd","dv","iv","model")
ggplot(m1res, aes(x = mean, y = iv)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=mean+1.96*sd,xmin=mean-1.96*sd),height=.2) + 
    theme_bw() + facet_grid(dv ~ model) + 
    ggtitle("Change in Similarity with\nReference each Moral Foundation")
ggsave(filename = "fig/sim1ideol.pdf", width = 6, height = 4)



## model 2: morality and sophistication
eqSystem <- list(polknow = general ~ polknow + relig + educ + age + female + black + num_total
               , polmedia = general ~ polmedia + relig + educ + age + female + black + num_total
               , poldisc = general ~ poldisc + relig + educ + age + female + black + num_total)
m2a <- systemfit(eqSystem, "OLS", data = data_dfm)
m2b <- systemfit(eqSystem, "OLS", data = data_tfidf)
summary(m2a)
summary(m2b)

## model 3: skipped for now

## model 4: change in feeling thermometers
m4a <- NULL
m4b <- NULL
m4a[[1]] <- lm(eval_party ~ harm + fairness + authority + ingroup +
                   relig + educ + age + female + black + num_total, data = data_dfm)
m4a[[2]] <- lm(eval_party ~ harm + fairness + authority + ingroup + pid_dem + pid_rep +
                   relig + educ + age + female + black + num_total, data = data_dfm)
m4a[[3]] <- lm(eval_cand ~ harm + fairness + authority + ingroup + pid_dem + pid_rep +
                   relig + educ + age + female + black + num_total, data = data_dfm)
m4a[[4]] <- lm(eval_cand ~ harm + fairness + authority + ingroup + pid_dem + pid_rep +
                   relig + educ + age + female + black + num_total, data = data_dfm)
m4b[[1]] <- lm(eval_party ~ harm + fairness + authority + ingroup +
                   relig + educ + age + female + black + num_total, data = data_tfidf)
m4b[[2]] <- lm(eval_party ~ harm + fairness + authority + ingroup + pid_dem + pid_rep +
                   relig + educ + age + female + black + num_total, data = data_tfidf)
m4b[[3]] <- lm(eval_cand ~ harm + fairness + authority + ingroup + pid_dem + pid_rep +
                   relig + educ + age + female + black + num_total, data = data_tfidf)
m4b[[4]] <- lm(eval_cand ~ harm + fairness + authority + ingroup + pid_dem + pid_rep +
                   relig + educ + age + female + black + num_total, data = data_tfidf)
lapply(m4a, summary)
lapply(m4b, summary)

## model 5: mft -> vote democratic
m5a <- NULL
m5b <- NULL
m5a[[1]] <- glm(vote_dem ~ harm + fairness + authority + ingroup +
               + relig + educ + age + female + black
             , data=data_dfm, family = binomial("logit"))
m5a[[2]] <- glm(vote_dem ~ harm + fairness + authority + ingroup +
               + pid_dem + pid_rep + relig + educ + age + female + black
             , data=data_dfm, family = binomial("logit"))
m5b[[1]] <- glm(vote_dem ~ harm + fairness + authority + ingroup +
               + relig + educ + age + female + black
             , data=data_tfidf, family = binomial("logit"))
m5b[[2]] <- glm(vote_dem ~ harm + fairness + authority + ingroup +
               + pid_dem + pid_rep + relig + educ + age + female + black
             , data=data_tfidf, family = binomial("logit"))
lapply(m5a, summary)
lapply(m5b, summary)

## model 6: mft -> turnout
m6a <- NULL
m6b <- NULL
m6a[[1]] <- glm(vote ~ harm + fairness + authority + ingroup +
                + relig + educ + age + female + black + num_total
              , data=data_dfm, family=binomial("logit"))
m6a[[2]] <- glm(vote ~ harm + fairness + authority + ingroup +
                + pid_str + relig + educ + age + female + black + num_total
              , data=data_dfm, family=binomial("logit"))
m6b[[1]] <- glm(vote ~ harm + fairness + authority + ingroup +
                + relig + educ + age + female + black + num_total
              , data=data_tfidf, family=binomial("logit"))
m6b[[2]] <- glm(vote ~ harm + fairness + authority + ingroup +
                + pid_str + relig + educ + age + female + black + num_total
              , data=data_tfidf, family=binomial("logit"))
lapply(m6a, summary)
lapply(m6b, summary)

capture.output(summary(m1a)
             , summary(m1b)
             , summary(m2a)
             , summary(m2b)
             , lapply(m4a, summary)
             , lapply(m4b, summary)
             , lapply(m5a, summary)
             , lapply(m5b, summary)
             , lapply(m6a, summary)
             , lapply(m6b, summary)
             , file = "out/output.txt"
)

## model 2: dirichlet model
m1raw <- data_dfm %>% dplyr::select(harm, fairness, authority, ingroup, purity
                                   , ideol_lib, ideol_con, relig, educ, age, female, black
                                  , num_total) %>% na.omit()
## model with Y matrix:Y = m1raw[,c("harm", "fairness", "authority", "ingroup", "purity")]
m1dl <- list(Y = m1raw[,"authority"] + .001
           , X = m1raw[,c("ideol_lib", "ideol_con", "relig", "educ", "age", "female", "black"
                        , "num_total")], I = nrow(m1raw), J = 8
           , X_new = data.frame(ideol_lib = c(1,0), ideol_con = c(0,1), relig = mean(m1raw$relig)
                              , educ = mean(m1raw$educ), age = mean(m1raw$educ)
                              , female = mean(m1raw$female), black = mean(m1raw$black)
                              , num_total = mean(m1raw$num_total)))
m1stan <- stan(file = "func/mft_beta.stan", data = m1dl)
print(m1stan, pars="gamma")


