###########################################################################################
## Project:  Measuring Morality in Political Attitude Expression
## File:     analyses_lisurvey.R
## Overview: main analyses (LI survey data) for the paper, produces plot
##           based on data prepared in prep_lisurvey.R
## Author:   Patrick Kraft
###########################################################################################

## packages
pkg <- c("tidyverse","foreign","car","quanteda",
         "gridExtra","stargazer","xtable","VGAM")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load recoded dataset
load("out/prep_lisurvey.RData")

## load additional functions
source("func.R")

## drop empty responses
lidat <- lidat[lidat$wc > 5, ]
lidat_lib <- lidat_lib[lidat_lib$wc > 5, ]
lidat_con <- lidat_con[lidat_con$wc > 5, ]


###########################
### Replicate main analyses

### Moral foundations in open-ended responses

## prepare data for plotting
plot_df <- lidat %>% select(purity_d, authority_d, ingroup_d, fairness_d, harm_d) %>%
  apply(2,function(x) c(mean(x, na.rm=T),sd(x, na.rm=T)/sqrt(sum(!is.na(x))))) %>%
  t() %>% data.frame() %>% mutate(var = rownames(.), varnum = as.factor(1:5))

## generate plot
ggplot(plot_df, aes(x=X1, xmin=X1-1.96*X2, xmax=X1+1.96*X2, y=varnum)) +
  geom_point(size=.5) + geom_errorbarh(height=0) + xlim(-.002,.5) +
  labs(y = "Moral Foundation", x = "Proportion of Respondents") +
  #ggtitle("Moral Reasoning in Open-Ended Responses") + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_discrete(labels=c("Sanctity", mftLabs))
ggsave(file = "fig/prop_lisurvey.pdf", width = 2, height = 1.5)


### Fig 10: Ideological differences in moral foundations (tobit, LI survey data)

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
  #ggtitle("Change in Predicted Emphasis on Moral Foundation (Replication)") +
  labs(y = "Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/tobit_ideol_lisurvey.pdf", width = 4, height = 2)


### Additional analyses: 
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



################
### save results

save.image(file="out/analyses_lisurvey.RData")
