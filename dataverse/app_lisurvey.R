###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     app_lisurvey.R
## Overview: Replication of main analysis using LI survey data (results in appendix)
## Requires: - 2001-2003 Long Island RDD survey data provided by Leonie Huddy
##              (data_lisurvey.rda, original data available on request)
##           - Custom auxiliary functions (func.R)
## Author:   Patrick Kraft
###########################################################################################

## packages
library(tidyverse)
library(gridExtra)
library(VGAM)

## load additional functions
source("func.R")

## load data
load("data_lisurvey.rda")



#######################
### figures in appendix

### Figure B.7: Proportion of individuals who mention moral foundations in open-ended responses

## prepare data for plotting
plot_df <- lidat %>% select(purity_d, authority_d, ingroup_d, fairness_d, harm_d) %>%
  apply(2,function(x) c(mean(x, na.rm=T),sd(x, na.rm=T)/sqrt(sum(!is.na(x))))) %>%
  t() %>% data.frame() %>% mutate(var = rownames(.), varnum = as.factor(1:5))

## generate plot
ggplot(plot_df, aes(x=X1, xmin=X1-1.96*X2, xmax=X1+1.96*X2, y=varnum)) +
  geom_point(size=.5) + geom_errorbarh(height=0) + xlim(-.002,.5) +
  labs(y = "Moral Foundation", x = "Proportion of Respondents") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_discrete(labels=c("Sanctity", mftLabs))
ggsave(file = "fig/appB7_prop_lisurvey.pdf", width = 2, height = 1.5)


### Fig C.1: Ideological differences in moral foundations (tobit, LI survey data)

## model estimation
tobit_ideol_li <- list(NULL)
tobit_ideol_li[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                            , tobit(Lower = 0), data = lidat)
tobit_ideol_li[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                            , tobit(Lower = 0), data = lidat)
tobit_ideol_li[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                            , tobit(Lower = 0), data = lidat)
tobit_ideol_li[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                            , tobit(Lower = 0), data = lidat)
lapply(tobit_ideol_li, summary)

## simulated expected values / marginal effects
tobit_ideol_li_res <- sim(tobit_ideol_li
                          , iv=data.frame(ideolModerate = c(0,0), ideolConservative = c(1,0)))
tobit_ideol_li_res$var <- rep(4:1, each=2)

## generate plot
ggplot(tobit_ideol_li_res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  labs(y = "Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/appC1_tobit_ideol_lisurvey.pdf", width = 4, height = 2)



#######################
### Additional analyses
##  - difference is most pronounced when talking about foundation relevant to group

## model estimation
tobit_ideol_lilib <- list(NULL)
tobit_ideol_lilib[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                               , tobit(Lower = 0), data = lidat_lib)
tobit_ideol_lilib[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                               , tobit(Lower = 0), data = lidat_lib)
tobit_ideol_lilib[[3]] <- try(vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                                   , tobit(Lower = 0), data = lidat_lib))
tobit_ideol_lilib[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                               , tobit(Lower = 0), data = lidat_lib)
lapply(tobit_ideol_lilib, summary)

## model estimation
tobit_ideol_licon <- list(NULL)
tobit_ideol_licon[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                               , tobit(Lower = 0), data = lidat_con)
tobit_ideol_licon[[2]] <- try(vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                                   , tobit(Lower = 0), data = lidat_con))
tobit_ideol_licon[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                               , tobit(Lower = 0), data = lidat_con)
tobit_ideol_licon[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                               , tobit(Lower = 0), data = lidat_con)
lapply(tobit_ideol_licon, summary)



###########################
### Descriptive information


### Fig B.8: Histograms of variables included in the analyses (LI survey data)

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
pdf("fig/appB8_lidesc.pdf", width=5, height=7)
grid.arrange(grobs=desc,ncol=2)
dev.off()



#############################
### Tables of Model Estimates


### Table D.4: Ideological differences in moral foundations (tobit, LI survey data)

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
           , file="tab/tabD4_tobit_ideol_lisurvey.tex"
           , table.placement="ht", caption.placement="top"
           , size="footnotesize")

