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
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")
covLabs <- c("Church Attendance","Education (College Degree)","Age","Sex (Female)"
             ,"Race (African American)","Number of Words")



###############################
### Plots/analyses in paper ###
###############################




######################################################
### Part 1: Ideological differences in moral reasoning


### Figure 1: Moral foundations and ideology

## generate plot
prop_plot(data=list(anes2012)
          , mftvarnames=c("purity_d", "authority_d", "ingroup_d", "fairness_d", "harm_d")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology"
          , file = "fig/fig1prop.pdf", width = 6, height = 4)


### Figure 2: ideology -> mft (tobit)

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

m2res <- sim(m2, iv=data.frame(ideolModerate = c(0,0), ideolConservative = c(1,0)))
m2res$var <- rep(4:1, each=2)

ggplot(m2res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cilo,xmin=cihi),height=0) +
  labs(y = "Dependent Variable: Moral Foundation"
       , x = "Marginal Effect") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(~value)

ggsave(filename = "fig/fig2ideol.pdf", width = 6, height = 4)


###########################################
### Part 2: Determinants of moral reasoning


### Figure 3: campaign exposure -> general mft reference (tobit)

## model estimation
m3 <- list(NULL)
m3[[1]] <- vglm(general_s ~ polknow + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[2]] <- vglm(general_s ~ polmedia + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[3]] <- vglm(general_s ~ poldisc + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[4]] <- vglm(general_s ~ polknow + polmedia + poldisc
                + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)

## simulation of predicted probabilities / first differences
m3_res <- rbind(sim(m3[[1]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[2]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[3]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T))))
m3_res$cond <- rep(c("No", "Yes"), each=6)
m3_res$var <- rep(c(3:1,3:1),each=2)
m3_res$year <- "2012"

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Independent Variable", x= "Marginal Effect") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Predicted Emphasis on\nany Moral Foundation") +
  guides(lty=guide_legend(title="Control for both remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_linetype_manual(values=c(1,2)) + facet_grid(~value, scales = "free_x")
ggsave(filename = "fig/fig3learn.pdf", width = 6, height = 4)


### Figure 3: engagement -> general mft reference (tobit)

## model estimation
m3 <- list(NULL)
m3[[1]] <- vglm(general_s ~ polknow + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[2]] <- vglm(general_s ~ polmedia + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[3]] <- vglm(general_s ~ poldisc + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)
m3[[4]] <- vglm(general_s ~ polknow + polmedia + poldisc
                + relig + educ + age + female + black + lwc + mode
                , tobit(Lower = 0), data=anes2012)

## simulation of predicted probabilities / first differences
m3_res <- rbind(sim(m3[[1]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[2]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[3]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T))))
m3_res$cond <- rep(c("No", "Yes"), each=6)
m3_res$var <- rep(c(3:1,3:1),each=2)
m3_res$year <- "2012"

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Independent Variable", x= "Marginal Effect") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Predicted Emphasis on\nany Moral Foundation") +
  guides(lty=guide_legend(title="Control for both remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_linetype_manual(values=c(1,2)) + facet_grid(~value, scales = "free_x")
ggsave(filename = "fig/fig3learn.pdf", width = 6, height = 4)



### Figure 4: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m4_know <- NULL
m4_know[[1]] <- lm(harm_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_know[[2]] <- lm(fairness_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_know[[3]] <- lm(ingroup_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_know[[4]] <- lm(authority_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_media <- NULL
m4_media[[1]] <- lm(harm_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                     , data=anes2012)
m4_media[[2]] <- lm(fairness_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                     , data=anes2012)
m4_media[[3]] <- lm(ingroup_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                     , data=anes2012)
m4_media[[4]] <- lm(authority_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                     , data=anes2012)
m4_disc <- NULL
m4_disc[[1]] <- lm(harm_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_disc[[2]] <- lm(fairness_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_disc[[3]] <- lm(ingroup_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_disc[[4]] <- lm(authority_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                    , data=anes2012)
m4_all <- NULL
m4_all[[1]] <- lm(harm_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                   , data=anes2012)
m4_all[[2]] <- lm(fairness_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                   , data=anes2012)
m4_all[[3]] <- lm(ingroup_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                   , data=anes2012)
m4_all[[4]] <- lm(authority_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                   , data=anes2012)


## simulation of predicted probabilities / difference-in-difference
m4_res <- rbind(sim(models = m4_know
                    , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T),2)
                                    , ideolModerate = rep(0,4)
                                    , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_media
                      , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_disc
                      , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_all
                      , iv=data.frame(polknow_c=rep(range(anes2012$polknow_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_all
                      , iv=data.frame(polmedia_c=rep(range(anes2012$polmedia_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_all
                      , iv=data.frame(poldisc_c=rep(range(anes2012$poldisc_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1))))
m4_res$var <- rep(rep(3:1,each=4),2)
m4_res$cond <- rep(c("No","Yes"), each = 12)
m4_res$year <- "2012"
levels(m4_res$dv) <- gsub("\n", "", rev(mftLabs))

## generate plot
ggplot(m4_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Effect of Ideology on the\nEmphasis of each Moral Foundation") +
  guides(lty=guide_legend(title="Control for Both Remaining Variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv) +
  scale_linetype_manual(values=c(1,2))
ggsave(filename = "fig/fig4ideolearn.pdf", width = 6, height = 5)



####################################################################
### Part 3: Investigating the political relevance of moral reasoning


### Figure 5: mft -> turnout (logit)

## model estimation
m5 <- NULL
m5[[1]] <- glm(vote ~ harm_s + fairness_s + ingroup_s + authority_s
               + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[2]] <- glm(vote ~ harm_s + fairness_s + ingroup_s + authority_s
               + pid_str + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m5_res <- rbind(sim(m5, iv=data.frame(harm_s = min(anes2012$harm_s)+c(0,1)))
                , sim(m5, iv=data.frame(fairness_s = min(anes2012$fairness_s)+c(0,1)))
                , sim(m5, iv=data.frame(ingroup_s = min(anes2012$ingroup_s)+c(0,1)))
                , sim(m5, iv=data.frame(authority_s = min(anes2012$authority_s)+c(0,1))))
m5_res$cond <- rep(c("No","Yes"),4)
m5_res$var <- rep(4:1,each=2)
m5_res$year <- "2012"

## generate plot
ggplot(m5_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Probability") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to\nParticipate in Election") +
  guides(lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_linetype_manual(values=c(1,2))
ggsave(filename = "fig/fig5turnout.pdf", width = 6, height = 4)


### Figure 6: mft -> protest behavior index (ols)

## model estimation
m6 <- NULL
m6[[1]] <- lm(part ~ harm_s + fairness_s + ingroup_s + authority_s
              + relig + educ + age + female + black + num_total, data=anes2012)
m6[[2]] <- lm(part ~ harm_s + fairness_s + ingroup_s + authority_s
              + pid_str + relig + educ + age + female + black + num_total, data=anes2012)

## simulation of predicted probabilities / first differences
m6_res <- rbind(sim(m6, iv=data.frame(harm_s = min(anes2012$harm_s)+c(0,1)), robust=T)
                , sim(m6, iv=data.frame(fairness_s = min(anes2012$fairness_s)+c(0,1)), robust=T)
                , sim(m6, iv=data.frame(ingroup_s = min(anes2012$ingroup_s)+c(0,1)), robust=T)
                , sim(m6, iv=data.frame(authority_s = min(anes2012$authority_s)+c(0,1)), robust=T))
m6_res$cond <- rep(c("No","Yes"),4)
m6_res$var <- rep(4:1,each=2)
m6_res$year <- "2012"

## generate plot
ggplot(m6_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Protest Index") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + ggtitle("Change in Protest Behavior Index") +
  guides(lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_y_continuous(breaks=1:4, labels=mftLabs) +
  scale_linetype_manual(values=c(1,2))
ggsave(filename = "fig/fig6part.pdf", width = 6, height = 4)


### New analyses for political relevance

m5 <- NULL
m5[[1]] <- glm(vote ~ general_s + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[2]] <- glm(vote ~ general_s + pid_str + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[3]] <- glm(protest ~ general_s + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[4]] <- glm(protest ~ general_s + pid_str + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[5]] <- glm(petition ~ general_s + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[6]] <- glm(petition ~ general_s + pid_str + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[7]] <- glm(button ~ general_s + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
m5[[8]] <- glm(button ~ general_s + pid_str + relig + educ + age + female + black + num_total
               , data=anes2012, family=binomial("logit"))
lapply(m5, summary)

## simulation of predicted probabilities / first differences
m5_res <- rbind(sim(m5, iv=data.frame(general_s = min(anes2012$general_s)+c(0,1))))
m5_res$cond <- rep(c("No","Yes"),4)
m5_res$var <- rep(4:1,each=2)
m5_res$year <- "2012"

## generate plot
ggplot(m5_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Dependent Variable:", x= "Change in Probability") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=1:4, labels=c("Button","Petition","Protest","Vote")) +
  ggtitle("Change in Predicted Probabilities to\nParticipate in Political Action") +
  guides(lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_linetype_manual(values=c(1,2))
ggsave(filename = "fig/fig5turnout.pdf", width = 6, height = 4)




### Figure 7: mft -> feeling thermometer differentials (ols)

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
ggplot(m7_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Independent Variable: Moral Foundation"
       , x= "Change in Feeling Thermometer (Democrat - Republican)") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + ggtitle("Change in Feeling Thermometer Differentials") +
  guides(lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(dv ~.) +
  scale_linetype_manual(values=c(1,2))
ggsave(filename = "fig/fig7feel.pdf", width = 6, height = 5)


### Figure 8: mft -> vote democratic (logit)

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
ggplot(m8_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) +
  labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Probability") +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to Vote\nfor Democratic Candidate") +
  guides(lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_linetype_manual(values=c(1,2))
ggsave(filename = "fig/fig8vote.pdf", width = 6, height = 4)




#################################
### Appendix: Data Overview ###
#################################


### table for missing cases

## prepare table
tab_mis <- rbind(c(table(anes2012$spanish)[2]
                     , table(anes2012$spanish)[2]*100/sum(table(anes2012$spanish)))
                 , c(table(anes2012$wc==0)[2]
                     , table(anes2012$wc==0)[2]*100/sum(table(anes2012$wc==0))))
colnames(tab_mis) <- c("N","Percent")
rownames(tab_mis) <- c("Spanish Interview", "No Responses")

## export table
print(xtable(tab_mis, align="lcc",digits=c(0,0,2)
             , caption = "Missing open-ended responses"
             , label="tab:appB1mis"),file="tab/appB1mis.tex")


### plot number of words (for wc>0!)

## histogram/density of wc
wc_mean = mean(anes2012$wc[anes2012$wc>0])
p1 <- ggplot(anes2012[anes2012$wc>0,], aes(wc)) + 
  geom_histogram(fill = "grey", binwidth = 25) + 
  theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, linetype = 3) +
  ylab("Number of Respondents") + xlab("Word Count")

## histogram/density of lwc
lwc_mean = mean(anes2012$lwc[anes2012$lwc>0])
p2 <- ggplot(anes2012[anes2012$lwc>0,], aes(lwc, ..density..)) + 
  geom_histogram(binwidth = 0.2, fill='grey') + geom_density() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = lwc_mean, linetype = 3) + 
  ylab("Density") + xlab("log(Word Count + 1)")

## combine plots
pdf("fig/wc.pdf",width=7, height=2)
grid.arrange(p1, p2, ncol=2)
dev.off()


