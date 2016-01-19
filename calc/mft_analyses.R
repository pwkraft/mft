###########################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     mft_analyses.R
## Overview: this file contains the main analyses and generates all plots and tables
##           for the paper. Uses the datasets generated in mft_prep
## Author:   Patrick Kraft
## Date:     12/08/2014
###########################################################################################


rm(list=ls())
setwd("/data/Uni/projects/2014/mft/calc")

## load packages
pkg <- c("ggplot2","stargazer","xtable")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(list=ls())

## load additional functions
source("func/anes_plot.R")

## load recoded dataset
load("out/anes.RData")



###############################
### Plots/analyses in paper ###
###############################


### labels for plots

mftLabs <- c("Authority / \nRespect", "Ingroup / \nLoyalty"
           , "Fairness / \nReciprocity", "Harm / \nCare")
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")


### Figure 1: Moral foundations and ideology

## generate plot
prop_plot(data=list(anes2012)
        , mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
        , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology"
        , file = "fig/p1_mft_ideol.pdf")


### Figure 2: ideology -> mft (logit)

## model estimation
m1 <- NULL
m1[[1]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m1[[2]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m1[[3]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m1[[4]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m1_res <- sim(m1, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m1_res$var <- factor(m1_res$dv, levels = rev(levels(m1_res$dv)), labels = mftLabs)

## generate plot
ggplot(m1_res, aes(x = mean, y = var)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation")
ggsave(filename = "fig/m1_mft.pdf")


### Figure 3: engagement -> general mft reference (logit)

## model estimation
m3 <- NULL
m3[[1]] <- glm(mft_all ~ polknow + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m3[[2]] <- glm(mft_all ~ polmedia + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m3[[3]] <- glm(mft_all ~ poldisc + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m3[[4]] <- glm(mft_all ~ polknow + polmedia + poldisc
               + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m3_res <- rbind(sim(m3[[1]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
              , sim(m3[[2]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
              , sim(m3[[3]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
              , sim(m3[[4]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
              , sim(m3[[4]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
              , sim(m3[[4]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T))))
m3_res$cond <- rep(c("No", "Yes"), each=3)
m3_res$var <- rep(3:1,2)

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable", x= "Change in Probability") + geom_vline(xintercept=0) + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference\nany Moral Foundation") +
    guides(lty=guide_legend(title="Control for both remaining variables")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=3:1, labels=polLabs)
ggsave(filename = "fig/m3_learn.pdf")


### Figure 4: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m3b_know <- NULL
m3b_know[[1]] <- glm(harm_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_know[[2]] <- glm(fair_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_know[[3]] <- glm(ingr_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_know[[4]] <- glm(auth_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_media <- NULL
m3b_media[[1]] <- glm(harm_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_media[[2]] <- glm(fair_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_media[[3]] <- glm(ingr_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_media[[4]] <- glm(auth_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_disc <- NULL
m3b_disc[[1]] <- glm(harm_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_disc[[2]] <- glm(fair_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_disc[[3]] <- glm(ingr_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_disc[[4]] <- glm(auth_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_all <- NULL
m3b_all[[1]] <- glm(harm_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m3b_all[[2]] <- glm(fair_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m3b_all[[3]] <- glm(ingr_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m3b_all[[4]] <- glm(auth_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))


## simulation of predicted probabilities / difference-in-difference
m3b_res <- rbind(sim(models = m3b_know
                   , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_media
                   , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_disc
                   , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_all
                   , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_all
                   , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_all
                   , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1))))
m3b_res$var <- rep(rep(3:1,each=4),2)
m3b_res$cond <- rep(c("No","Yes"), each = 12)
levels(m3b_res$dv) <- gsub("\n", "", rev(mftLabs))

## generate plot
ggplot(m3b_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
    geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Effect of Ideology on the\nProbability to Reference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for Both Remaining Variables")) +
    theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv) + 
    scale_y_continuous(breaks=3:1, labels=polLabs)
ggsave(filename = "fig/m3b_learn.pdf")


### Figure 5: mft -> turnout (logit)

## model estimation
m2b <- NULL
m2b[[1]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                + relig + educ + age + female + black + num_total
              , data=anes2012, family=binomial("logit"))
m2b[[2]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                + pid_str + relig + educ + age + female + black + num_total
              , data=anes2012, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m2b_res <- rbind(sim(m2b, iv=data.frame(harm_all = c(0,1)))
               , sim(m2b, iv=data.frame(fair_all = c(0,1)))
               , sim(m2b, iv=data.frame(ingr_all = c(0,1)))
               , sim(m2b, iv=data.frame(auth_all = c(0,1))))
m2b_res$cond <- rep(c("No","Yes"),4)
m2b_res$var <- rep(4:1,each=2)

## generate plot
ggplot(m2b_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") +
    geom_vline(xintercept=0) + theme_bw() +
    ggtitle("Change in Predicted Probabilities to Participate in Election") +
    guides(lty=guide_legend(title="Control for PID Strength")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/m2b_vote.pdf")


### Figure 6: mft -> protest behavior index (ols)

## model estimation
m2e <- NULL
m2e[[1]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black + num_total, data=anes2012)
m2e[[2]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
               + pid_str + relig + educ + age + female + black + num_total, data=anes2012)

## simulation of predicted probabilities / first differences
m2e_res <- rbind(sim(m2e, iv=data.frame(harm_all = c(0,1)), robust=T)
               , sim(m2e, iv=data.frame(fair_all = c(0,1)), robust=T)
               , sim(m2e, iv=data.frame(ingr_all = c(0,1)), robust=T)
               , sim(m2e, iv=data.frame(auth_all = c(0,1)), robust=T))
m2e_res$cond <- rep(c("No","Yes"),4)
m2e_res$var <- rep(4:1,each=2)

## generate plot
ggplot(m2e_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable: Moral Foundation", x= "Change in Protest Index") +
    geom_vline(xintercept=0) + theme_bw() + ggtitle("Change in Protest Behavior Index") +
    guides(lty=guide_legend(title="Control for PID Strength")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/m2e_part.pdf")


### Figure 7: mft -> feeling thermometer differentials (ols)

## model estimation
m2f <- NULL
m2f[[1]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black, data=anes2012)
m2f[[2]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
               + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012)
m2f[[3]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black, data=anes2012)
m2f[[4]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
               + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012)

## simulation of predicted probabilities / first differences
m2f_res <- rbind(sim(m2f, iv=data.frame(harm_all = c(0,1)), robust=T)
               , sim(m2f, iv=data.frame(fair_all = c(0,1)), robust=T)
               , sim(m2f, iv=data.frame(ingr_all = c(0,1)), robust=T)
               , sim(m2f, iv=data.frame(auth_all = c(0,1)), robust=T))
m2f_res$cond <- rep(c("No","Yes"),8)
m2f_res$var <- rep(4:1,each=4)
levels(m2f_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(m2f_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable: Moral Foundation", x= "Change in Feeling Thermometer") +
    geom_vline(xintercept=0) + theme_bw() + ggtitle("Change in Feeling Thermometer Differentials") +
    guides(lty=guide_legend(title="Control for Party Identification")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(dv ~.)
ggsave(filename = "fig/m2f_vote.pdf")


### Figure 8: mft -> vote democratic (logit)

## model estimation
m2 <- NULL
m2[[1]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black
             , data=anes2012, family = binomial("logit"))
m2[[2]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
               + pid_dem + pid_rep + relig + educ + age + female + black
             , data=anes2012, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m2_res <- rbind(sim(m2, iv=data.frame(harm_all = c(0,1)))
               , sim(m2, iv=data.frame(fair_all = c(0,1)))
               , sim(m2, iv=data.frame(ingr_all = c(0,1)))
               , sim(m2, iv=data.frame(auth_all = c(0,1))))
m2_res$cond <- rep(c("No","Yes"),4)
m2_res$var <- rep(4:1,each=2)

## generate plot
ggplot(m2_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") +
    geom_vline(xintercept=0) + theme_bw() +
    ggtitle("Change in Predicted Probabilities to Vote for Democratic Candidate") +
    guides(lty=guide_legend(title="Control for Party Identification")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/m2_vote.pdf")



#############################
### Material for Appendix ###
#############################
