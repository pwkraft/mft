###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     analyses.R
## Overview: analyses for the paper, produces all plots and tables
##           based on data prepared in prep.R
## Author:   Patrick Kraft
###############################################################################################

## packages
pkg <- c("dplyr","ggplot2","stargazer","xtable","VGAM","pmisc")
invisible(lapply(pkg, library, character.only = TRUE))
rm(list=ls())

## working directory
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load additional functions
source("func.R")

## load recoded dataset
load("out/anes.RData")

## global labels for plots
mftLabs <- c("Authority / \nRespect", "Ingroup / \nLoyalty"
             , "Fairness / \nReciprocity", "Harm / \nCare")
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions"
             , "Voted\nin 2008","Protest\nBehavior")
covLabs <- c("Church Attendance","Education (College Degree)","Age","Sex (Female)"
             ,"Race (African American)","Number of Words")

## drop spanish respondents and empty responses
anes2012 <- anes2012[anes2012$spanish != 1 & anes2012$wc != 0,]


##############################################
### Ideological differences in moral reasoning


### Moral foundations and ideology

prop_plot(data=list(anes2012)
          , mftvarnames=c("purity_d", "authority_d", "ingroup_d", "fairness_d", "harm_d")
          , groupvarname="ideol", legendname = NULL, title = "Moral Foundations and Ideology"
          , file = "fig/prop_ideol.pdf", width = 3, height = 3)


### ideology -> mft (tobit)

## model estimation: tobit (could also use the censReg or AER packages...)
m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + lwc + mode
              , tobit(Lower = 0), data = anes2012)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + lwc + mode
              , tobit(Lower = 0), data = anes2012)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + lwc + mode
              , tobit(Lower = 0), data = anes2012)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + lwc + mode
              , tobit(Lower = 0), data = anes2012)
lapply(m2, summary)

## simulated expected values / marginal effects
m2res <- sim(m2, iv=data.frame(ideolModerate = c(0,0), ideolConservative = c(1,0)))
m2res$var <- rep(4:1, each=2)

## generate plot
ggplot(m2res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  ggtitle("Change in Predicted Emphasis on Moral Foundation") +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)
ggsave(filename = "fig/tobit_ideol.pdf", width = 5, height = 2.5)


### mft -> feeling thermometer differentials (ols)

## model estimation
m7 <- NULL
m7[[1]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
              + relig + educ + age + female + black, data=anes2012)
m7[[2]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
              + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012)
m7[[3]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
              + relig + educ + age + female + black, data=anes2012)
m7[[4]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
              + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012)

## simulation of predicted probabilities / first differences
m7_res <- rbind(sim(m7, iv=data.frame(harm_s = min(anes2012$harm_s)+c(0,1)), robust=T)
                , sim(m7, iv=data.frame(fairness_s = min(anes2012$fairness_s)+c(0,1)), robust=T)
                , sim(m7, iv=data.frame(ingroup_s = min(anes2012$ingroup_s)+c(0,1)), robust=T)
                , sim(m7, iv=data.frame(authority_s = min(anes2012$authority_s)+c(0,1)), robust=T))
m7_res$cond <- rep(c("No","Yes"),8)
m7_res$var <- rep(4:1,each=4)
m7_res$year <- "2012"
levels(m7_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(m7_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
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


### mft -> vote democratic (logit)

## model estimation
m8 <- NULL
m8[[1]] <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
               + relig + educ + age + female + black
               , data=anes2012, family = binomial("logit"))
m8[[2]] <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
               + pid_dem + pid_rep + relig + educ + age + female + black
               , data=anes2012, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m8_res <- rbind(sim(m8, iv=data.frame(harm_s = min(anes2012$harm_s)+c(0,1)))
                , sim(m8, iv=data.frame(fairness_s = min(anes2012$fairness_s)+c(0,1)))
                , sim(m8, iv=data.frame(ingroup_s = min(anes2012$ingroup_s)+c(0,1)))
                , sim(m8, iv=data.frame(authority_s = min(anes2012$authority_s)+c(0,1))))
m8_res$cond <- rep(c("No","Yes"),4)
m8_res$var <- rep(4:1,each=2)
m8_res$year <- "2012"

## generate plot
ggplot(m8_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to\nVote for Democratic Candidate") +
  guides(col=guide_legend(title="Control for Party Identification")
         , shape=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/logit_vote.pdf", width = 3, height = 3)


###################################
### Determinants of moral reasoning


### campaign exposure -> general mft reference (tobit)

## model estimation
m3 <- list(NULL)
m3[[1]] <- vglm(general_s ~ polknow + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[2]] <- vglm(general_s ~ polmedia + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[3]] <- vglm(general_s ~ poldisc + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[4]] <- vglm(general_s ~ pastvote + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[5]] <- vglm(general_s ~ part + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[6]] <- vglm(general_s ~ polknow + polmedia + poldisc + pastvote + part
                + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
lapply(m3, summary)

## simulation of predicted probabilities / first differences
m3_res <- rbind(sim(m3[[1]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[2]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[3]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(pastvote=range(anes2012$pastvote, na.rm = T)))
                , sim(m3[[5]], iv=data.frame(part=range(anes2012$part, na.rm = T)))
                , sim(m3[[6]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[6]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[6]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                , sim(m3[[6]], iv=data.frame(pastvote=range(anes2012$pastvote, na.rm = T)))
                , sim(m3[[6]], iv=data.frame(part=range(anes2012$part, na.rm = T))))
m3_res$cond <- rep(c("No", "Yes"), each=10)
m3_res$var <- rep(c(5:1,5:1),each=2)
m3_res$year <- "2012"

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Independent Variable", x= "Marginal Effect") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=5:1, labels=polLabs) +
  ggtitle("Change in Predicted Emphasis on any Moral Foundation") +
  guides(col=guide_legend(title="Control for remaining variables")
         , shape=guide_legend(title="Control for remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_grey(start=0,end=.5) + facet_grid(~value, scales = "free_x")
ggsave(filename = "fig/tobit_learn.pdf", width = 5, height = 3)



### engagement/sophistication X ideology -> specific mft reference (tobit)

## model estimation
m4_know <- list(NULL)
m4_know[[1]] <- vglm(harm_s ~ ideol*polknow_c + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_know[[2]] <- vglm(fairness_s ~ ideol*polknow_c + relig + educ + age + female + black + 
                     lwc + mode, tobit(Lower = 0), data=anes2012)
m4_know[[3]] <- vglm(ingroup_s ~ ideol*polknow_c + relig + educ + age + female + black +
                   lwc + mode, tobit(Lower = 0), data=anes2012)
m4_know[[4]] <- vglm(authority_s ~ ideol*polknow_c + relig + educ + age + female + black +
                   lwc + mode, tobit(Lower = 0), data=anes2012)
m4_media <- list(NULL)
m4_media[[1]] <- vglm(harm_s ~ ideol*polmedia_c + relig + educ + age + female + black + 
                      lwc + mode, tobit(Lower = 0), data=anes2012)
m4_media[[2]] <- vglm(fairness_s ~ ideol*polmedia_c + relig + educ + age + female + black + 
                      lwc + mode, tobit(Lower = 0), data=anes2012)
m4_media[[3]] <- vglm(ingroup_s ~ ideol*polmedia_c + relig + educ + age + female + black + 
                      lwc + mode, tobit(Lower = 0), data=anes2012)
m4_media[[4]] <- vglm(authority_s ~ ideol*polmedia_c + relig + educ + age + female + black + 
                      lwc + mode, tobit(Lower = 0), data=anes2012)
m4_disc <- list(NULL)
m4_disc[[1]] <- vglm(harm_s ~ ideol*poldisc_c + relig + educ + age + female + black +
                   lwc + mode, tobit(Lower = 0), data=anes2012)
m4_disc[[2]] <- vglm(fairness_s ~ ideol*poldisc_c + relig + educ + age + female + black + 
                     lwc + mode, tobit(Lower = 0), data=anes2012)
m4_disc[[3]] <- vglm(ingroup_s ~ ideol*poldisc_c + relig + educ + age + female + black + 
                     lwc + mode, tobit(Lower = 0), data=anes2012)
m4_disc[[4]] <- vglm(authority_s ~ ideol*poldisc_c + relig + educ + age + female + black + 
                     lwc + mode, tobit(Lower = 0), data=anes2012)
m4_vote <- list(NULL)
m4_vote[[1]] <- vglm(harm_s ~ ideol*vote + relig + educ + age + female + black +
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_vote[[2]] <- vglm(fairness_s ~ ideol*vote + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_vote[[3]] <- vglm(ingroup_s ~ ideol*vote + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_vote[[4]] <- vglm(authority_s ~ ideol*vote + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_part <- list(NULL)
m4_part[[1]] <- vglm(harm_s ~ ideol*part + relig + educ + age + female + black +
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_part[[2]] <- vglm(fairness_s ~ ideol*part + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_part[[3]] <- vglm(ingroup_s ~ ideol*part + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_part[[4]] <- vglm(authority_s ~ ideol*part + relig + educ + age + female + black + 
                       lwc + mode, tobit(Lower = 0), data=anes2012)
m4_all <- list(NULL)
m4_all[[1]] <- vglm(harm_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c + 
                    ideol*vote + ideol*part + relig + educ + age + female + black + 
                    lwc + mode, tobit(Lower = 0), data=anes2012)
m4_all[[2]] <- vglm(fairness_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c +
                    ideol*vote + ideol*part + relig + educ + age + female + black + 
                    lwc + mode, tobit(Lower = 0), data=anes2012)
m4_all[[3]] <- vglm(ingroup_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c +
                    ideol*vote + ideol*part + relig + educ + age + female + black + 
                    lwc + mode, tobit(Lower = 0), data=anes2012)
m4_all[[4]] <- vglm(authority_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c +
                    ideol*vote + ideol*part + relig + educ + age + female + black + 
                    lwc + mode, tobit(Lower = 0), data=anes2012)

## simulation of predicted probabilities / difference-in-difference
m4_res <- rbind(sim(models = m4_know
                    , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T),each=2)
                                    , ideolModerate = rep(0,4)
                                    , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_media
                      , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_disc
                      , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_vote
                      , iv=data.frame(vote=rep(range(anes2012$vote, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_part
                      , iv=data.frame(part=rep(range(anes2012$part, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_all
                      , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_all
                      , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_all
                      , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_all
                      , iv=data.frame(vote=rep(range(anes2012$vote, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1)))
                , sim(models = m4_all
                      , iv=data.frame(part=rep(range(anes2012$part, na.rm = T),each=2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,1,0,1))))
m4_res$var <- rep(rep(5:1,each=8),2)
m4_res$cond <- rep(c("No","Yes"), each = 40)
m4_res$year <- "2012"
levels(m4_res$dv) <- gsub("\n", "", rev(mftLabs))

## generate plot
ggplot(m4_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), col=cond, shape=cond)) +
  geom_vline(xintercept=0, col="lightgrey") + geom_point() +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=0) +
  labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  scale_y_continuous(breaks=5:1, labels=polLabs) +
  ggtitle("Change in Effect of Ideology on the\nEmphasis of each Moral Foundation") +
  guides(col=guide_legend(title="Control for remaining variables")
         , shape=guide_legend(title="Control for remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + facet_grid(dv~value) +
  scale_color_grey(start=0,end=.5)
ggsave(filename = "fig/tobit_learnideol.pdf", width = 4, height = 6)



