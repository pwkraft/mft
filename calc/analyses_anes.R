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


### Fig 1: Moral foundations in open-ended responses

## prepare data for plotting
plot_df <- anes2012 %>% select(purity_d, authority_d, ingroup_d, fairness_d, harm_d) %>%
  apply(2,function(x) c(mean(x, na.rm=T),sd(x, na.rm=T)/sqrt(sum(!is.na(x))))) %>%
  t() %>% data.frame() %>% mutate(var = rownames(.), varnum = as.factor(1:5))

## generate plot
ggplot(plot_df, aes(x=X1, xmin=X1-1.96*X2, xmax=X1+1.96*X2, y=varnum)) +
  geom_point() + geom_errorbarh(height=0) + xlim(0,.5) +
  labs(y = "Moral Foundation", x = "Proportion of Respondents") +
  ggtitle("Moral Reasoning in Open-Ended Responses") + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_discrete(labels=c("Purity / \nSanctity", mftLabs))
ggsave(file = "fig/prop_mft.pdf", width = 5, height = 2)


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
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/tobit_ideol.pdf", width = 5, height = 2.5)



##############################################
### The Political Relevance of Moral Reasoning


### Fig 3: Moral foundations and feeling thermometer differentials (ols)

## model estimation
ols_feel <- NULL
ols_feel[[1]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
                    + relig + educ + age + female + black + lwc + wordsum + mode
                    , data=anes2012)
ols_feel[[2]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
                    + pid_dem + pid_rep + relig + educ + age + female + black
                    + lwc + wordsum + mode, data=anes2012)
ols_feel[[3]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
                    + relig + educ + age + female + black + lwc + wordsum + mode
                    , data=anes2012)
ols_feel[[4]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
                    + pid_dem + pid_rep + relig + educ + age + female + black
                    + lwc + wordsum + mode, data=anes2012)

## simulate expected values / marginal effects
ols_feel_res <- rbind(sim(ols_feel, iv=data.frame(harm_s = min(anes2012$harm_s)+c(0,1))
                          , robust=T)
                      , sim(ols_feel, iv=data.frame(fairness_s = min(anes2012$fairness_s)+c(0,1))
                            , robust=T)
                      , sim(ols_feel, iv=data.frame(ingroup_s = min(anes2012$ingroup_s)+c(0,1))
                            , robust=T)
                      , sim(ols_feel, iv=data.frame(authority_s = min(anes2012$authority_s)+c(0,1))
                            , robust=T))
ols_feel_res$cond <- rep(c("No","Yes"),8)
ols_feel_res$var <- rep(4:1,each=4)
ols_feel_res$year <- "2012"
levels(ols_feel_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(ols_feel_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable: Moral Foundation"
       , x= "Change in Feeling Thermometer (Democrat - Republican)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  ggtitle("Change in Feeling Thermometer Differentials") +
  guides(col=guide_legend(title="Control for Party Identification")
         , shape=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_wrap(~dv) +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/ols_feel.pdf", width = 5, height = 3)


### Fig 4: Moral foundations and democratic vote (logit)

## model estimation
logit_vote <- NULL
logit_vote[[1]] <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
                       + relig + educ + age + female + black + lwc + wordsum + mode
                       , data=anes2012, family = binomial("logit"))
logit_vote[[2]] <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
                       + pid_dem + pid_rep + relig + educ + age + female + black 
                       + lwc + wordsum + mode, data=anes2012, family = binomial("logit"))

## simulation of predicted probabilities / first differences
logit_vote_res <- rbind(sim(logit_vote, iv=data.frame(harm_s = c(0,1)))
                        , sim(logit_vote, iv=data.frame(fairness_s = c(0,1)))
                        , sim(logit_vote, iv=data.frame(ingroup_s = c(0,1)))
                        , sim(logit_vote, iv=data.frame(authority_s = c(0,1))))
logit_vote_res$cond <- rep(c("No","Yes"),4)
logit_vote_res$var <- rep(4:1,each=2)
logit_vote_res$year <- "2012"

## generate plot
ggplot(logit_vote_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to\nVote for Democratic Candidate") +
  guides(col=guide_legend(title="Control for Party Identification")
         , shape=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/logit_vote.pdf", width = 3, height = 3)



#########################################
### The Conditionality of Moral Reasoning


### Fig 5: Knoledge/media/discussion and general moral reasoning (tobit)

## model estimation
tobit_learn <- list(NULL)
tobit_learn[[1]] <- vglm(general_s ~ polknow + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_learn[[2]] <- vglm(general_s ~ polmedia + relig + educ + age + female + black
                         + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_learn[[3]] <- vglm(general_s ~ poldisc + relig + educ + age + female + black 
                         + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_learn[[4]] <- vglm(general_s ~ polknow + polmedia + poldisc
                         + relig + educ + age + female + black + lwc + wordsum + mode
                         , tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_learn_res <- rbind(sim(tobit_learn[[1]]
                             , iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                         , sim(tobit_learn[[2]]
                               , iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                         , sim(tobit_learn[[3]]
                               , iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                         , sim(tobit_learn[[4]]
                               , iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                         , sim(tobit_learn[[4]]
                               , iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                         , sim(tobit_learn[[4]]
                               , iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T))))
tobit_learn_res$cond <- rep(c("No", "Yes"), each=6)
tobit_learn_res$var <- rep(c(3:1,3:1),each=2)
tobit_learn_res$year <- "2012"

## generate plot
ggplot(tobit_learn_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable", x= "Marginal Effect") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Predicted Emphasis on any Moral Foundation") +
  guides(col=guide_legend(title="Control for remaining variables")
         , shape=guide_legend(title="Control for remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5) + facet_grid(~value)
ggsave(filename = "fig/tobit_learn.pdf", width = 5, height = 3)


### Fig 6: Knowledge and ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol_know <- list(NULL)
tobit_ideol_know[[1]] <- vglm(harm_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_know[[2]] <- vglm(fairness_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_know[[3]] <- vglm(ingroup_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_know[[4]] <- vglm(authority_s ~ ideol*polknow_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- rbind(sim(models = tobit_ideol_know
                             , iv=data.frame(polknow_c=min(anes2012$polknow_c, na.rm = T)
                                             , ideolModerate = c(0,0)
                                             , ideolConservative = c(1,0)))
                         , sim(models = tobit_ideol_know
                               , iv=data.frame(polknow_c=max(anes2012$polknow_c, na.rm = T)
                                               , ideolModerate = c(0,0)
                                               , ideolConservative = c(1,0))))
tobit_ideol_res$var <- factor(tobit_ideol_res$dv)
levels(tobit_ideol_res$dv) <- rev(mftLabs)
tobit_ideol_res$cond <- rep(c("Low Knowledge","High Knowledge"),each=8)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = dv)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(cond~value) + 
  scale_y_discrete(limits = rev(levels(tobit_ideol_res$dv)))
ggsave(filename = "fig/tobit_ideol_know.pdf", width = 4, height = 3)


### Fig 7: Media exposure and ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol_media <- list(NULL)
tobit_ideol_media[[1]] <- vglm(harm_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_media[[2]] <- vglm(fairness_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_media[[3]] <- vglm(ingroup_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_media[[4]] <- vglm(authority_s ~ ideol*polmedia_c + relig + educ + age + female + black 
                               + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- rbind(sim(models = tobit_ideol_media
                             , iv=data.frame(polmedia_c=min(anes2012$polmedia_c, na.rm = T)
                                             , ideolModerate = c(0,0)
                                             , ideolConservative = c(1,0)))
                         , sim(models = tobit_ideol_media
                               , iv=data.frame(polmedia_c=max(anes2012$polmedia_c, na.rm = T)
                                               , ideolModerate = c(0,0)
                                               , ideolConservative = c(1,0))))
tobit_ideol_res$var <- factor(tobit_ideol_res$dv)
levels(tobit_ideol_res$dv) <- rev(mftLabs)
tobit_ideol_res$cond <- rep(c("Low Media Exposure","High Media Exposure"),each=8)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = dv)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(cond~value) + 
  scale_y_discrete(limits = rev(levels(tobit_ideol_res$dv)))
ggsave(filename = "fig/tobit_ideol_media.pdf", width = 4, height = 3)


### Fig 8: Political discussion and ideological differences in moral foundations (tobit)

## model estimation
tobit_ideol_disc <- list(NULL)
tobit_ideol_disc[[1]] <- vglm(harm_s ~ ideol*poldisc_c + relig + educ + age + female + black
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_disc[[2]] <- vglm(fairness_s ~ ideol*poldisc_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_disc[[3]] <- vglm(ingroup_s ~ ideol*poldisc_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_ideol_disc[[4]] <- vglm(authority_s ~ ideol*poldisc_c + relig + educ + age + female + black 
                              + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)

## simulate expected values / marginal effects
tobit_ideol_res <- rbind(sim(models = tobit_ideol_disc
                             , iv=data.frame(poldisc_c=min(anes2012$poldisc_c, na.rm = T)
                                             , ideolModerate = c(0,0)
                                             , ideolConservative = c(1,0)))
                         , sim(models = tobit_ideol_disc
                               , iv=data.frame(poldisc_c=max(anes2012$poldisc_c, na.rm = T)
                                               , ideolModerate = c(0,0)
                                               , ideolConservative = c(1,0))))
tobit_ideol_res$var <- factor(tobit_ideol_res$dv)
levels(tobit_ideol_res$dv) <- rev(mftLabs)
tobit_ideol_res$cond <- rep(c("Low Discussion Frequency","High Discussion Frequency"),each=8)

## generate plot
ggplot(tobit_ideol_res, aes(x = mean, y = dv)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(cond~value) + 
  scale_y_discrete(limits = rev(levels(tobit_ideol_res$dv)))
ggsave(filename = "fig/tobit_ideol_disc.pdf", width = 4, height = 3)


### Fig 9: Media content and moral foundations (tobit)

## model estimation
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_fairness + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_ingroup + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_authority + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## simulate expected values / marginal effects
tobit_cont_res <- rbind(sim(models = tobit_cont[[1]]
                            , iv=data.frame(media_harm=range(anes2012$media_harm), polmedia = mean(anes2012$polmedia, na.rm = T)))
                        , sim(models = tobit_cont[[2]]
                              , iv=data.frame(media_fairness=range(anes2012$media_fairness), polmedia = mean(anes2012$polmedia, na.rm = T)))
                        , sim(models = tobit_cont[[3]]
                              , iv=data.frame(media_ingroup=range(anes2012$media_ingroup), polmedia = mean(anes2012$polmedia, na.rm = T)))
                        , sim(models = tobit_cont[[4]]
                              , iv=data.frame(media_authority=range(anes2012$media_authority), polmedia = mean(anes2012$polmedia, na.rm = T))))
tobit_cont_res$var <- factor(tobit_cont_res$dv)
levels(tobit_cont_res$var) <- rev(mftLabs)

## generate plot
ggplot(tobit_cont_res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (MFT Media Content)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  facet_grid(.~value) + 
  scale_y_discrete(limits = rev(levels(tobit_cont_res$var)))
ggsave("fig/tobit_cont.pdf", width = 5, height = 2.5)

## placebo test: individualizing vs. binding foundations
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_fairness + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_harm + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_authority + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_ingroup + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## placebo test: other random foundations
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_ingroup + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_authority + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_harm + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_fairness + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## including all controls
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm + media_fairness + media_ingroup + media_authority + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_harm + media_fairness + media_ingroup + media_authority + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_harm + media_fairness + media_ingroup + media_authority + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_harm + media_fairness + media_ingroup + media_authority + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## binding vs. individualizing
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(I(harm_s + fairness_s) ~ I(media_harm + media_fairness) + I(media_ingroup + media_authority) + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(I(ingroup_s + authority_s) ~ I(media_harm + media_fairness) + I(media_ingroup + media_authority) + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## checking alternative recodings
tobit_cont <- list(NULL)
tobit_cont[[1]] <- vglm(harm_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[2]] <- vglm(fairness_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[3]] <- vglm(ingroup_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
tobit_cont[[4]] <- vglm(authority_s ~ media_harm_s + media_fairness_s + media_ingroup_s + media_authority_s + relig + educ + age + female + black 
                        + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
lapply(tobit_cont, summary)

## try general mft instead
m <- vglm(general_s ~ media_general*polmedia + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m)

m <- vglm(general_s ~ media_general_s*polmedia + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m)

## try different media sources
m <- vglm(general_s ~ wkinews*inews_general + wktvnws*tvnws_general
          + wkpaprnws*paprnws_general + wkrdnws*rdnws_general
          + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m)

summary(vglm(general_s ~ wkinews*inews_general + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(general_s ~ wktvnws*tvnws_general + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(general_s ~ wkpaprnws*paprnws_general + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(general_s ~ wkrdnws*rdnws_general + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))

summary(vglm(general_s ~ wkinews*inews_general_s + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(general_s ~ wktvnws*tvnws_general_s + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(general_s ~ wkpaprnws*paprnws_general_s + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))
summary(vglm(general_s ~ wkrdnws*rdnws_general_s + relig + educ + age + female + black 
             + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012))

## try different media sources
m <- vglm(general_s ~ wkinews*inews_general_s + wktvnws*tvnws_general_s
          + wkpaprnws*paprnws_general_s + wkrdnws*rdnws_general_s
          + relig + educ + age + female + black 
          + lwc + wordsum + mode, tobit(Lower = 0), data=anes2012)
summary(m)


################
### save results

save.image(file="out/analyses_anes.RData")