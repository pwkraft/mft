###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     lisurvey.R
## Overview: preliminary analyses of LI survey data
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("foreign","car","dplyr","quanteda","tidyr")
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
lidat$ideol <- recode(lidat$ideol, "'liberal' = '1. Liberal'; 'moderate'='2. Moderate'; 'conservative'='3. Conservative'; else=NA")
#levels(lidat$ideol) <- c("Liberal","Moderate","Conservative")

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

dat <- merge(lidat, lisim) %>% mutate(year = "all responses")
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

dat_lib <- merge(lidat, lisim_lib) %>% mutate(year = "liberals")
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")

dat_con <- merge(lidat, lisim_con) %>% mutate(year = "conservatives")
dat %>% group_by(ideol) %>% summarise_each(authority_s, fairness_s, harm_s, ingroup_s, purity_s, funs="mean")


prop_plot(data=list(dat,dat_lib,dat_con)
          , mftvarnames=c("purity_d", "authority_d", "ingroup_d", "fairness_d", "harm_d")
          , groupvarname="ideol", legendname = NULL, title = "Moral Foundations and Ideology"
          , file = "fig/prop_lisurvey.pdf", width = 3, height = 3, lim=c(-.01,.25))

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

plot_df <- rbind(dplyr::filter(dat_lib,ideol=="1. Liberal"),dplyr::filter(dat_con,ideol=="3. Conservative")) %>% 
  select(year, authority_s, ingroup_s, fairness_s, harm_s) %>%
  gather(mft, score, -year) %>% group_by(year,mft) %>% summarise_each(funs(mean,se))

ggplot(plot_df, aes(x=mean, xmin=mean-1.96*se, xmax=mean+1.96*se, y=year)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~mft) +
  labs(y = "Ideology", x = "Proportion of Respondents") +
  ggtitle("Moral Reasoning in Open-Ended Responses") + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA))
#ggsave(file = "fig/prop_lisurvey_libcon.pdf", width = 4, height = 2)

