###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     lisurvey.R
## Overview: preliminary analyses of LI survey data
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("tidyverse","foreign","car","quanteda",
         "gridExtra","stargazer","xtable","VGAM","pmisc")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## data directory
datsrc <- "~/Dropbox/Uni/Data/"

## global labels for plots
mftLabs <- c("Authority / \nRespect", "Ingroup / \nLoyalty"
             , "Fairness / \nReciprocity", "Harm / \nCare")
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")
covLabs <- c("Church Attendance","Education (College Degree)","Age","Sex (Female)"
             ,"Race (African American)","Number of Words")

## load data
raw <- read.dta(paste0(datsrc,"lisurvey/combined123.dta"))


###########################
### recode time-series data

## respondent id
raw$id <- rownames(raw)
lidat <- data.frame(id = raw$id)

## ideology (factor/dummies)
lidat$ideol <- recode(raw$ideol, "'liberal' = 'Liberal'; 'moderate'='Moderate'; 'conservative'='Conservative'; else=NA")
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

## pre-process open-ended data and calculate similarity
lisim <- mftSimilarity(opend = select(raw, deslib,deslibb,descon,desconb)
                       , id = raw$id, dict = dict, regex = dict_df, dict_list = dict_list)
lisim_lib <- mftSimilarity(opend = select(raw, deslib,deslibb)
                           , id = raw$id, dict = dict, regex = dict_df, dict_list = dict_list)
lisim_con <- mftSimilarity(opend = select(raw, descon,desconb)
                       , id = raw$id, dict = dict, regex = dict_df, dict_list = dict_list)

apply(lisim_lib[-2],2,function(x) mean(as.numeric(x))) - apply(lisim_con[-2],2,function(x) mean(as.numeric(x)))

dat <- merge(lidat, lisim) %>% mutate(year = "all responses") %>% filter(wc>0)
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

dat_lib <- merge(lidat, lisim_lib) %>% mutate(year = "liberals") %>% filter(wc>0)
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

dat_con <- merge(lidat, lisim_con) %>% mutate(year = "conservatives") %>% filter(wc>0)
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")


#####################
### Descriptive plots

prop_plot(data=list(dat,dat_lib,dat_con)
          , mftvarnames=c("purity_d", "authority_d", "ingroup_d", "fairness_d", "harm_d")
          , groupvarname="ideol", legendname = NULL, title = "Moral Foundations and Ideology"
          , file = "fig/prop_lisurvey.pdf", width = 3, height = 3, lim=c(-.01,.31))

se <- function(x){
  sd(x, na.rm=T)/sqrt(sum(!is.na(x)))
}

plot_df <- rbind(dat_lib,dat_con) %>% 
  select(year, authority_s, ingroup_s, fairness_s, harm_s) %>%
  gather(mft, score, -year) %>% group_by(year,mft) %>% summarise_each(funs(mean,se))

ggplot(plot_df, aes(x=mean, xmin=mean-1.96*se, xmax=mean+1.96*se, y=year)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~mft) +
  labs(y = "Ideology", x = "Proportion of Respondents") +
  ggtitle("Moral Reasoning in Open-Ended Responses") + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA))
ggsave(file = "fig/prop_lisurvey_libcon.pdf", width = 4, height = 2)

plot_df <- rbind(dplyr::filter(dat_lib,ideol=="Liberal"),dplyr::filter(dat_con,ideol=="Conservative")) %>% 
  select(year, authority_s, ingroup_s, fairness_s, harm_s) %>%
  gather(mft, score, -year) %>% group_by(year,mft) %>% summarise_each(funs(mean,se))

ggplot(plot_df, aes(x=mean, xmin=mean-1.96*se, xmax=mean+1.96*se, y=year)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~mft) +
  labs(y = "Ideology", x = "Proportion of Respondents") +
  ggtitle("Moral Reasoning in Open-Ended Responses") + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA))
#ggsave(file = "fig/prop_lisurvey_libcon.pdf", width = 4, height = 2)


###########################
### Replicate main analyses

m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat)
lapply(m2, summary)

## simulated expected values / marginal effects
m2res <- sim(m2, iv=data.frame(ideolModerate = c(0,0), ideolConservative = c(1,0)))
m2res$var <- rep(4:1, each=2)

## generate plot
ggplot(m2res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation (Replication)") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/tobit_ideol_lisurvey.pdf", width = 5, height = 2.5)


m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_lib)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_lib)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_lib)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_lib)
lapply(m2, summary)


m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_con)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_con)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_con)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = dat_con)
lapply(m2, summary)

