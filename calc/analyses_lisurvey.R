###########################################################################################
## Project:  Moral Foundations of Political Reasoning
## File:     analyses_lisurvey.R
## Overview: main analyses (LI survey data) for the paper, produces plot
##           based on data prepared in prep_lisurvey.R
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
m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat)
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


### Additional analyses: 
##  - difference is most pronounced when talking about foundation relevant to group

## model estimation
m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_lib)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_lib)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_lib)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_lib)
lapply(m2, summary)

## model estimation
m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_con)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_con)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_con)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc
                , tobit(Lower = 0), data = lidat_con)
lapply(m2, summary)



################
### save results

save.image(file="out/analyses_lisurvey.RData")
