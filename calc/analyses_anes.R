###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     analyses_anes.R
## Overview: main analyses (anes data) for the paper, produces plots and tables
##           based on data prepared in prep_anes.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("tidyverse","gridExtra","stargazer","xtable","VGAM")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("~/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/prep_anes.RData")

## drop spanish respondents and empty responses
anes2012 <- anes2012[anes2012$spanish != 1 & anes2012$wc != 0, ]



##############################################
### Ideological Differences in Moral Reasoning


### Fig 2: Ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol <- list(NULL)
tobit_ideol[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data = anes2012)
tobit_ideol[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data = anes2012)
tobit_ideol[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data = anes2012)
tobit_ideol[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data = anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- sim(tobit_ideol, iv=data.frame(ideolModerate = c(0,0)
                                                  , ideolConservative = c(1,0)))
tobit_ideol_res$var <- rep(4:1, each=2)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  #ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/tobit_ideol.pdf", width = 4, height = 2)



##############################################
### The Political Relevance of Moral Reasoning


### Fig 4: Moral foundations and democratic vote (logit)

## model estimation
logit_vote <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
                  + pid_dem + pid_rep + relig + educ + age + female + black 
                  + lwc + wordsum + mode, data=anes2012, family = binomial("logit"))

## simulation of predicted probabilities / first differences
logit_vote_res <- rbind(sim(logit_vote, iv=data.frame(harm_s = c(0,1)))
                        , sim(logit_vote, iv=data.frame(fairness_s = c(0,1)))
                        , sim(logit_vote, iv=data.frame(ingroup_s = c(0,1)))
                        , sim(logit_vote, iv=data.frame(authority_s = c(0,1))))
logit_vote_res$var <- 4:1

## generate plot
ggplot(logit_vote_res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Moral Foundation", x= "Change in P(democratic vote)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) +
  #ggtitle("Change in Predicted Probabilities to\nVote for Democratic Candidate") +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/logit_vote.pdf", width = 2.5, height = 2)



#########################################
### The Conditionality of Moral Reasoning


### Fig 5: Media content effects (tobit)

## model estimation
tobit_media <- vglm(general_s ~ media_general + polmedia_c + poldisc + polknow
                    + relig + educ + age + female + black + lwc + wordsum + mode
                    , tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_media_res <- sim(tobit_media, iv=data.frame(media_general=seq(min(anes2012$media_general, na.rm = T)
                                                                    , max(anes2012$media_general, na.rm = T)
                                                                    , length.out=20)),nsim=2000)

## check diff
sim(tobit_media, iv=data.frame(media_general=range(anes2012$media_general, na.rm = T)))

## generate plot
ggplot(tobit_media_res, aes(x=ivval, y=mean, ymin=cilo,ymax=cihi)) +
  geom_ribbon(alpha=0.1) + geom_line() + facet_wrap(~value, scale="free_y") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
  # ggtitle("Media Content Effects") +
  labs(y = "Moral Reasoning", x= "Moral Media Content")
ggsave(filename = "fig/tobit_media.pdf", width = 4, height = 2)





################
### save results

save.image(file="out/analyses_anes.RData")