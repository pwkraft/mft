###########################################################################################
## Project:  Moral Foundations of Political Reasoning
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

## load additional functions
source("func.R")

## load recoded dataset
load("out/prep_lisurvey.RData")

## drop empty responses
lidat <- lidat[lidat$wc != 0, ]
lidat_lib <- lidat_lib[lidat_lib$wc != 0, ]
lidat_con <- lidat_con[lidat_con$wc != 0, ]


###########################
### Replicate main analyses


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
  ggtitle("Change in Predicted Emphasis on Moral Foundation (Replication)") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/tobit_ideol_lisurvey.pdf", width = 5, height = 2.5)


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
