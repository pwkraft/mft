###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     appendix_lisurvey.R
## Overview: analyses for appendix (lisurvey data), produces all additional plots and 
##           tables based on data prepared in prep_lisurvey.R and models estimated in 
##           analyses_lisurvey.R
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

## load recoded dataset
load("out/analyses_lisurvey.RData")

## drop empty responses
lidat <- lidat[lidat$wc > 5, ]
lidat_lib <- lidat_lib[lidat_lib$wc > 5, ]
lidat_con <- lidat_con[lidat_con$wc > 5, ]



######################################
### Additional Descriptive information


### Fig B.4: Histograms of variables included in the analyses (LI survey data)

desc <- list(NULL)
plot_default <- theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA))
desc[[1]] <- ggplot(lidat, aes(x=ideol)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology") + plot_default
desc[[2]] <- ggplot(lidat, aes(x=age)) + geom_bar(stat="count") + 
  labs(y="Count", x="Age") + plot_default
desc[[3]] <- ggplot(lidat, aes(x=factor(female,labels=c("Male","Female")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Sex") + plot_default
desc[[4]] <- ggplot(lidat, aes(x=factor(black,labels=c("Other","Black non-Hispanic")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Race/Ethnicity") + plot_default
desc[[5]] <- ggplot(lidat, aes(x=relig)) + geom_bar(stat="count") + 
  labs(y="Count", x="Church Attendance") + plot_default
desc[[6]] <- ggplot(lidat, aes(x=factor(educ, labels=c("No College","College")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Education") + plot_default
desc[[7]] <- ggplot(lidat, aes(x=wc)) + geom_histogram(binwidth = 5) + 
  labs(y="Count", x="Word Count") + plot_default
desc[[8]] <- ggplot(lidat, aes(x=lwc)) + geom_histogram(binwidth = .2) + 
  labs(y="Count", x="log(Word Count)") + plot_default
pdf("fig/app_lidesc.pdf", width=5, height=7)
grid.arrange(grobs=desc,ncol=2)
dev.off()



#############################
### Tables of Model Estimates


### Fig 10: Ideological differences in moral foundations (tobit, LI survey data)

## print summary
lapply(tobit_ideol_li, summary)

## create labels
varlabs = list(ideolConservative="Ideology (Conservative)", ideolModerate="Ideology (Moderate)"
               , relig="Church Attendance", educ="Education (College Degree)"
               , age="Age", female1="Sex (Female)", black="Race (African American)"
               , lwc="Word Count (log)"
               , "(Intercept):1"="Intercept", "(Intercept):2"="log(Sigma)")
mlabs <- c("Care", "Fairness", "Loyalty", "Authority")

## create table
latexTable(tobit_ideol_li, caption="Tobit models predicting MFT score for each foundation based 
           on ideology (telephone survey replication). Positive coefficients indicate stronger emphasis on the respective 
           foundation. Standard errors in parentheses. Estimates are used for Figure 
           \\ref{fig:tobit_ideol_lisurvey} in the main text."
           , label="tab:tobit_ideol_lisurvey", align="lcccc"
           , varlabs=varlabs, mlabs=mlabs
           , file="tab/tobit_ideol_lisurvey.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")


