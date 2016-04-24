###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     mft_analyses.R
## Overview: new analysis focusing on cosine similarity b/w dictionaries and responses
##           uses the datasets generated in mft_prep
## Author:   Patrick Kraft
###############################################################################################


rm(list=ls())
setwd("/data/Dropbox/Uni/Projects/2014/mft/calc")

## load packages
pkg <- c("ggplot2","stargazer","xtable","quanteda","systemfit","dplyr","rstan","VGAM")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(list=ls())
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## load additional functions
source("func/anes_plot.R")

## load recoded dataset
load("out/anes.RData")

## global labels for plots
mftLabs <- c("Authority / \nRespect", "Ingroup / \nLoyalty"
             , "Fairness / \nReciprocity", "Harm / \nCare")
polLabs <- c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")
covLabs <- c("Church Attendance","Education (College Degree)","Age","Sex (Female)"
             ,"Race (African American)","Number of Words")

## create scaled variables for moral foundations (throws an error if run twice, not sure why)
anes2012 <- mutate(anes2012, harm_s = scale(harm, center = FALSE), fairness_s = scale(fairness, center = FALSE)
                   , ingroup_s = scale(ingroup, center = FALSE), authority_s = scale(authority, center = FALSE)
                   , purity_s = scale(purity, center = FALSE), general_s = scale(general, center = FALSE))
anes2008 <- mutate(anes2008, harm_s = scale(harm, center = FALSE), fairness_s = scale(fairness, center = FALSE)
                   , ingroup_s = scale(ingroup, center = FALSE), authority_s = scale(authority, center = FALSE)
                   , purity_s = scale(purity, center = FALSE), general_s = scale(general, center = FALSE))




###############################
### Plots/analyses in paper ###
###############################




######################################################
### Part 1: Ideological differences in moral reasoning


### Figure 1: Moral foundations and ideology

## generate plot
prop_plot(data=list(anes2012)
          , mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology"
          , file = "fig/fig1prop.pdf", width = 6, height = 4)


### Figure 2: ideology -> mft (tobit)

## model estimation: tobit (could also use the censReg or AER packages...)
m2 <- list(NULL)
m2[[1]] <- vglm(harm_s ~ ideol + relig + educ + age + female + black + num_total
              , tobit(Lower = 0), data = anes2012)
m2[[2]] <- vglm(fairness_s ~ ideol + relig + educ + age + female + black + num_total
              , tobit(Lower = 0), data = anes2012)
m2[[3]] <- vglm(ingroup_s ~ ideol + relig + educ + age + female + black + num_total
              , tobit(Lower = 0), data = anes2012)
m2[[4]] <- vglm(authority_s ~ ideol + relig + educ + age + female + black + num_total
              , tobit(Lower = 0), data = anes2012)
lapply(m2, summary)

iv=data.frame(ideol=c(0,1))

## model estimation (SUR)
eqSystem <- list(harm = harm_s ~ ideol + relig + educ + age + female + black + num_total
               , fairness = fairness_s ~ ideol + relig + educ + age + female + black + num_total
               , ingroup = ingroup_s ~ ideol + relig + educ + age + female + black + num_total
               , authority = authority_s ~ ideol + relig + educ + age + female + black + num_total)
m2 <- systemfit(eqSystem, "OLS", data = anes2012)
m2res <- coef(summary(m2))
m2res <- data.frame(m2res[grep("Conservative",rownames(m2res)),1:2])
colnames(m2res) <- c("mean","se")
m2res$var <- 4:1
m2res$year <- "2012"

## generate plot
ggplot(m2res, aes(x = -mean, y = var)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) + xlim(-0.4,0.4) + 
  geom_errorbarh(aes(xmax=-mean+1.96*se,xmin=-mean-1.96*se),height=.2) + 
  labs(y = "Dependent Variable:\nMoral Foundation"
       , x = "Change in Similarity Score (in standard deviations)") + 
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Emphasis on each Moral Foundation\nfor Liberals compared to Conservatives")  + 
  ggsave(filename = "fig/fig2ideol.pdf", width = 6, height = 4)



###########################################
### Part 2: Determinants of moral reasoning


### Figure 3: engagement -> general mft reference (OLS)

## model estimation
m3 <- NULL
m3[[1]] <- lm(general_s ~ polknow + relig + educ + age + female + black + num_total, data=anes2012)
m3[[2]] <- lm(general_s ~ polmedia + relig + educ + age + female + black + num_total, data=anes2012)
m3[[3]] <- lm(general_s ~ poldisc + relig + educ + age + female + black + num_total, data=anes2012)
m3[[4]] <- lm(general_s ~ polknow + polmedia + poldisc
               + relig + educ + age + female + black + num_total
               , data=anes2012)

## simulation of predicted probabilities / first differences
m3_res <- rbind(sim(m3[[1]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)), robust = T)
                , sim(m3[[2]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)), robust = T)
                , sim(m3[[3]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)), robust = T)
                , sim(m3[[4]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)), robust = T)
                , sim(m3[[4]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)), robust = T)
                , sim(m3[[4]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T), robust = T)))
m3_res$cond <- rep(c("No", "Yes"), each=3)
m3_res$var <- rep(3:1,2)
m3_res$year <- "2012"

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
  labs(y = "Independent Variable", x= "Change in Similarity Score (in standard deviations)") + 
  theme_bw() + scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Predicted Emphasis on\nany Moral Foundation") +
  guides(lty=guide_legend(title="Control for both remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal")
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
  theme_bw() + scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Effect of Ideology on the\nEmphasis of each Moral Foundation") +
  guides(lty=guide_legend(title="Control for Both Remaining Variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv)
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
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to\nParticipate in Election") +
  guides(lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal")
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
  theme_bw() + ggtitle("Change in Protest Behavior Index") +
  guides(lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=mftLabs)
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
  theme_bw() + scale_y_continuous(breaks=1:4, labels=c("Button","Petition","Protest","Vote")) +
  ggtitle("Change in Predicted Probabilities to\nParticipate in Political Action") +
  guides(lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal")
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
  theme_bw() + ggtitle("Change in Feeling Thermometer Differentials") +
  guides(lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(dv ~.)
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
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to Vote\nfor Democratic Candidate") +
  guides(lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal")
ggsave(filename = "fig/fig8vote.pdf", width = 6, height = 4)




#################################
### Appendix B: Data Overview ###
#################################


### table for missing cases

## prepare table
tab_mis <- rbind(c(table(anes2008$spanish)[2]
                   , table(anes2008$spanish)[2]*100/sum(table(anes2008$spanish)))
                 , c(table(anes2012$spanish)[2]
                     , table(anes2012$spanish)[2]*100/sum(table(anes2012$spanish)))
                 , c(table(anes2008$num_total==0)[2]
                     , table(anes2008$num_total==0)[2]*100/sum(table(anes2008$num_total==0)))
                 , c(table(anes2012$num_total==0)[2]
                     , table(anes2012$num_total==0)[2]*100/sum(table(anes2012$num_total==0)))
                 , c(table(anes2008$num_ca==0)[2]
                     , table(anes2008$num_ca==0)[2]*100/sum(table(anes2008$num_ca==0)))
                 , c(table(anes2012$num_ca==0)[2]
                     , table(anes2012$num_ca==0)[2]*100/sum(table(anes2012$num_ca==0)))
                 , c(table(anes2008$num_pa==0)[2]
                     , table(anes2008$num_pa==0)[2]*100/sum(table(anes2008$num_pa==0)))
                 , c(table(anes2012$num_pa==0)[2]
                     , table(anes2012$num_pa==0)[2]*100/sum(table(anes2012$num_pa==0))))
colnames(tab_mis) <- c("N","Percent")
rownames(tab_mis) <- c("Spanish Interview (2008)", "Spanish Interview (2012)"
                       , "No Responses (Overall, 2008)", "No Responses (Overall, 2012)"
                       , "No Responses (Candidate Evaluations, 2008)"
                       , "No Responses (Candidate Evaluations, 2012)"
                       , "No Responses (Party Evaluations, 2008)"
                       , "No Responses (Party Evaluations, 2012)")

## export table
print(xtable(tab_mis, align="lcc",digits=c(0,0,2)
             , caption = "Missing open-ended responses"
             , label="tab:appB1mis"),file="tab/appB1mis.tex")


### plot number of words (note that some max values are omitted)

## create individual plots
appB2a <- qplot(num_total, data=anes2012[anes2012$num_total>0, ], geom="bar", binwidth = 1
                , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2012)") +
  geom_vline(aes(xintercept = mean(num_total, na.rm=T)), linetype="dotted", size=1)
appB2b <- qplot(num_ca, data=anes2012[anes2012$num_ca>0, ], geom="bar", binwidth = 1
                , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2012)") +
  geom_vline(aes(xintercept = mean(num_ca, na.rm=T)), linetype="dotted", size=1)
appB2c <- qplot(num_pa, data=anes2012[anes2012$num_pa>0, ], geom="bar", binwidth = 1
                , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2012)") +
  geom_vline(aes(xintercept = mean(num_pa, na.rm=T)), linetype="dotted", size=1)
appB2d <- qplot(num_total, data=anes2008[anes2008$num_total>0, ], geom="bar", binwidth = 1
                , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2008)") +
  geom_vline(aes(xintercept = mean(num_total, na.rm=T)), linetype="dotted", size=1)
appB2e <- qplot(num_ca, data=anes2008[anes2008$num_ca>0, ], geom="bar", binwidth = 1
                , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2008)") +
  geom_vline(aes(xintercept = mean(num_ca, na.rm=T)), linetype="dotted", size=1)
appB2f <- qplot(num_pa, data=anes2008[anes2008$num_pa>0, ], geom="bar", binwidth = 1
                , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2008)") +
  geom_vline(aes(xintercept = mean(num_pa, na.rm=T)), linetype="dotted", size=1)


## save multiplot
pdf("fig/appB2num.pdf", width = 7, height = 5)
multiplot(appB2a, appB2b, appB2c, appB2d, appB2e, appB2f, cols=2)
dev.off()




################################################
### Appendix C: Additional descriptive plots ###
################################################


### proportion of respondents mentioning each moral foundation

## generate individual plots
appC1 <- prop_plot(data=list(anes2008,anes2012), groupvarname="ideol", legendname = "Ideology"
                   , mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
                   , title = "Moral Foundation and Ideology:\nAll Evaluations"
                   , file = "fig/appC1prop.pdf")
appC2 <- prop_plot(data=list(anes2008,anes2012), groupvarname="ideol", legendname = "Ideology"
                   , mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
                   , title = "Moral Foundation and Ideology:\nCandidate Evaluations"
                   , file = "fig/appC2cand.pdf")
appC3 <- prop_plot(data=list(anes2008,anes2012), groupvarname="ideol", legendname = "Ideology"
                   , mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
                   , title = "Moral Foundation and Ideology:\nParty Evaluations"
                   , file = "fig/appC3party.pdf")




##########################################################################
### Appendix D: Alternative model specifications and robustness checks ###
##########################################################################



#####################################################################
### Part 1: Ideological differences in moral reasoning including 2008


### Figure D1 [fig 2 + 2008]: ideology -> mft (SUR)

## model estimation (SUR)
eqSystem <- list(harm = harm_s ~ ideol + relig + educ + age + female + black + num_total
                 , fairness = fairness_s ~ ideol + relig + educ + age + female + black + num_total
                 , ingroup = ingroup_s ~ ideol + relig + educ + age + female + black + num_total
                 , authority = authority_s ~ ideol + relig + educ + age + female + black + num_total)
m2_2008 <- systemfit(eqSystem, "SUR", data = anes2008)
m2_2008res <- coef(summary(m2_2008))
m2_2008res <- data.frame(m2_2008res[grep("Conservative",rownames(m2_2008res)),1:2])
colnames(m2_2008res) <- c("mean","se")
m2_2008res$var <- 4:1
m2_2008res$year <- "2008"

## generate plot
ggplot(rbind(m2res,m2_2008res), aes(x = -mean, y=var-.052+.11*(year=="2008")
                                      , shape=year, color = year)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=4) + xlim(-0.4,0.4) + 
  geom_errorbarh(aes(xmax=-mean+1.96*se,xmin=-mean-1.96*se),height=.1) + 
  labs(y = "Dependent Variable:\nMoral Foundation"
       , x = "Change in Similarity Score (in standard deviations)") + 
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Emphasis on each Moral Foundation\nfor Conservatives compared to Liberals")  + 
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD1ideol.pdf", height = 5)




## alternative model: beta

## alternative model: dirichlet
m1raw <- anes2012 %>% dplyr::select(harm, fairness, authority, ingroup, purity
                                    , ideol_lib, ideol_con, relig, educ, age, female, black
                                    , num_total) %>% na.omit()
Y <- m1raw[,c("harm", "fairness", "authority", "ingroup")]
Y <- Y + min(Y[Y!=0])
Y <- cbind(Y, 1 - apply(Y,1,sum))
m1dl <- list(Y = Y, K = ncol(Y), init_r = c(-.5,.5)
             , X = m1raw[,c("ideol_lib", "ideol_con", "relig", "educ", "age", "female", "black"
                            , "num_total")], I = nrow(m1raw), J = 8
             , X_new = data.frame(ideol_lib = c(1,0), ideol_con = c(0,1), relig = mean(m1raw$relig)
                                  , educ = mean(m1raw$educ), age = mean(m1raw$educ)
                                  , female = mean(m1raw$female), black = mean(m1raw$black)
                                  , num_total = mean(m1raw$num_total)))
m1stan <- stan(file = "func/mft_dirichlet.stan", data = m1dl)
print(m1stan)



##########################################################
### Part 2: Determinants of moral reasoning including 2008


### Figure D7 [fig 3 + 2008]: engagement -> general mft reference (OLS)

## model estimation
m3_2008 <- NULL
m3_2008[[1]] <- lm(general_s ~ polknow + relig + educ + age + female + black + num_total, data=anes2008)
m3_2008[[2]] <- lm(general_s ~ polmedia + relig + educ + age + female + black + num_total, data=anes2008)
m3_2008[[3]] <- lm(general_s ~ poldisc + relig + educ + age + female + black + num_total, data=anes2008)
m3_2008[[4]] <- lm(general_s ~ polknow + polmedia + poldisc
              + relig + educ + age + female + black + num_total
              , data=anes2012)

## simulation of predicted probabilities / first differences
m3_2008_res <- rbind(sim(m3_2008[[1]], iv=data.frame(polknow=range(anes2008$polknow, na.rm = T)), robust = T)
                , sim(m3_2008[[2]], iv=data.frame(polmedia=range(anes2008$polmedia, na.rm = T)), robust = T)
                , sim(m3_2008[[3]], iv=data.frame(poldisc=range(anes2008$poldisc, na.rm = T)), robust = T)
                , sim(m3_2008[[4]], iv=data.frame(polknow=range(anes2008$polknow, na.rm = T)), robust = T)
                , sim(m3_2008[[4]], iv=data.frame(polmedia=range(anes2008$polmedia, na.rm = T)), robust = T)
                , sim(m3_2008[[4]], iv=data.frame(poldisc=range(anes2008$poldisc, na.rm = T)), robust = T))
m3_2008_res$cond <- rep(c("No", "Yes"), each=3)
m3_2008_res$var <- rep(3:1,2)
m3_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m3_res,m3_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                      , shape=year, color = year, lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable", x= "Change in Similarity Score (in standard deviations)") + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted  Emphasis on\nany Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for both remaining variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=3:1, labels=polLabs)
ggsave(filename = "fig/appD7learn.pdf", height = 6)


### Figure D8 [fig 4 + 2008]: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m4_2008_know <- NULL
m4_2008_know[[1]] <- lm(harm_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_know[[2]] <- lm(fairness_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_know[[3]] <- lm(ingroup_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_know[[4]] <- lm(authority_s ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_media <- NULL
m4_2008_media[[1]] <- lm(harm_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008)
m4_2008_media[[2]] <- lm(fairness_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008)
m4_2008_media[[3]] <- lm(ingroup_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008)
m4_2008_media[[4]] <- lm(authority_s ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008)
m4_2008_disc <- NULL
m4_2008_disc[[1]] <- lm(harm_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_disc[[2]] <- lm(fairness_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_disc[[3]] <- lm(ingroup_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_disc[[4]] <- lm(authority_s ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008)
m4_2008_all <- NULL
m4_2008_all[[1]] <- lm(harm_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                  + relig + educ + age + female + black + num_total
                  , data=anes2008)
m4_2008_all[[2]] <- lm(fairness_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                  + relig + educ + age + female + black + num_total
                  , data=anes2008)
m4_2008_all[[3]] <- lm(ingroup_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                  + relig + educ + age + female + black + num_total
                  , data=anes2008)
m4_2008_all[[4]] <- lm(authority_s ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                  + relig + educ + age + female + black + num_total
                  , data=anes2008)


## simulation of predicted probabilities / difference-in-difference
m4_2008_res <- rbind(sim(models = m4_2008_know
                    , iv=data.frame(polknow_c=rep(range(anes2008$polknow_c, na.rm = T),2)
                                    , ideolModerate = rep(0,4)
                                    , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_2008_media
                      , iv=data.frame(polmedia_c=rep(range(anes2008$polmedia_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_2008_disc
                      , iv=data.frame(poldisc_c=rep(range(anes2008$poldisc_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_2008_all
                      , iv=data.frame(polknow_c=rep(range(anes2008$polknow_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_2008_all
                      , iv=data.frame(polmedia_c=rep(range(anes2008$polmedia_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1)))
                , sim(models = m4_2008_all
                      , iv=data.frame(poldisc_c=rep(range(anes2008$poldisc_c, na.rm = T),2)
                                      , ideolModerate = rep(0,4)
                                      , ideolConservative = c(0,0,1,1))))
m4_2008_res$var <- rep(rep(3:1,each=4),2)
m4_2008_res$cond <- rep(c("No","Yes"), each = 12)
m4_2008_res$year <- "2008"
levels(m4_2008_res$dv) <- gsub("\n", "", rev(mftLabs))

## generate plot
ggplot(rbind(m4_res,m4_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                      , shape=year, color = year, lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
  theme_bw() + scale_y_continuous(breaks=3:1, labels=polLabs) +
  ggtitle("Change in Effect of Ideology on the\nEmphasis of each Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for Both Remaining Variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv) +
  scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD8ideolearn.pdf", height = 6)



#######################################################################################
### Part 3: Investigating the political relevance of political reasoning including 2008

### Figure D9 [fig 5 + 2008]: mft -> turnout (logit)

## model estimation
m5_2008 <- NULL
m5_2008[[1]] <- glm(vote ~ harm_s + fairness_s + ingroup_s + authority_s
                    + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m5_2008[[2]] <- glm(vote ~ harm_s + fairness_s + ingroup_s + authority_s
                    + pid_str + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m5_2008_res <- rbind(sim(m5_2008, iv=data.frame(harm_s = min(anes2008$harm_s)+c(0,1)))
                , sim(m5_2008, iv=data.frame(fairness_s = min(anes2008$fairness_s)+c(0,1)))
                , sim(m5_2008, iv=data.frame(ingroup_s = min(anes2008$ingroup_s)+c(0,1)))
                , sim(m5_2008, iv=data.frame(authority_s = min(anes2008$authority_s)+c(0,1))))
m5_2008_res$cond <- rep(c("No","Yes"),4)
m5_2008_res$var <- rep(4:1,each=2)
m5_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m5_res,m5_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                      , shape=year, color = year, lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Probability") +
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to\nParticipate in Election") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD9turnout.pdf", height = 6)


### Figure D11 [fig 7 + 2008]: mft -> feeling thermometer differentials (ols)

## model estimation
m7_2008 <- NULL
m7_2008[[1]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
                   + relig + educ + age + female + black, data=anes2008)
m7_2008[[2]] <- lm(eval_party ~ harm_s + fairness_s + ingroup_s + authority_s
                   + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008)
m7_2008[[3]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
                   + relig + educ + age + female + black, data=anes2008)
m7_2008[[4]] <- lm(eval_cand ~ harm_s + fairness_s + ingroup_s + authority_s
                   + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008)

## simulation of predicted probabilities / first differences
m7_2008_res <- rbind(sim(m7_2008, iv=data.frame(harm_s = min(anes2008$harm_s)+c(0,1)), robust=T)
                     , sim(m7_2008, iv=data.frame(fairness_s = min(anes2008$fairness_s)+c(0,1)), robust=T)
                     , sim(m7_2008, iv=data.frame(ingroup_s = min(anes2008$ingroup_s)+c(0,1)), robust=T)
                     , sim(m7_2008, iv=data.frame(authority_s = min(anes2008$authority_s)+c(0,1)), robust=T))
m7_2008_res$cond <- rep(c("No","Yes"),8)
m7_2008_res$var <- rep(4:1,each=4)
m7_2008_res$year <- "2008"
levels(m7_2008_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(rbind(m7_res, m7_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                       , shape=year, color = year, lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation"
       , x= "Change in Feeling Thermometer (Democrat - Republican)") +
  theme_bw() + ggtitle("Change in Feeling Thermometer Differentials") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(dv ~.) +
  scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD11feel.pdf", height = 6)


### Figure D12 [fig 8 + 2008]: mft -> vote democratic (logit)

## model estimation
m8_2008 <- NULL
m8_2008[[1]] <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
                    + relig + educ + age + female + black
                    , data=anes2008, family = binomial("logit"))
m8_2008[[2]] <- glm(vote_dem ~ harm_s + fairness_s + ingroup_s + authority_s
                    + pid_dem + pid_rep + relig + educ + age + female + black
                    , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m8_2008_res <- rbind(sim(m8_2008, iv=data.frame(harm_s = min(anes2008$harm_s)+c(0,1)))
                     , sim(m8_2008, iv=data.frame(fairness_s = min(anes2008$fairness_s)+c(0,1)))
                     , sim(m8_2008, iv=data.frame(ingroup_s = min(anes2008$ingroup_s)+c(0,1)))
                     , sim(m8_2008, iv=data.frame(authority_s = min(anes2008$authority_s)+c(0,1))))
m8_2008_res$cond <- rep(c("No","Yes"),4)
m8_2008_res$var <- rep(4:1,each=2)
m8_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m8_res,m8_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                      , shape=year, color = year, lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Probability") +
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Predicted Probabilities to Vote\nfor Democratic Candidate") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") +
  scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD12vote.pdf", height = 6)




#############################################
### Appendix E: Tables of model estimates ###
#############################################



#####################################################################
### Part 1: Ideological differences in moral reasoning including 2008


### Table for D1 [fig 2 + 2008]: ideology -> mft (logit)

stargazer(m2[[1]], m2_2008[[1]], m2[[2]], m2_2008[[2]]
          , m2[[3]], m2_2008[[3]], m2[[4]], m2_2008[[4]]
          , type="text", out="tab/m2ideol.tex"
          , title="Logit models predicting references to four moral foundations using ideology"
          , covariate.labels = c("Conservative", "Moderate", covLabs)
          , column.labels = rep(c("2012","2008"),4)
          , model.numbers = FALSE, order=c(2,1,3:8)
          , dep.var.labels = rev(gsub("\n","",mftLabs))
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2ideol", no.space=T, table.placement="ht"
)


### Tables for alternative model specifications


##########################################################
### Part 2: Determinants of moral reasoning including 2008


### Table for D7 [fig 3 + 2008]: engagement -> general mft reference (logit)

stargazer(m3, m3_2008
          , type="text", out="tab/m3learn.tex"
          , title="Logit models predicting overall references to any moral foundation"
          , covariate.labels = c("Political Knowledge","Political Media Exposure"
                                 ,"Political Discussion",covLabs)
          , column.labels = c("2012","2008"), column.separate = c(4,4)
          , model.numbers = FALSE
          , dep.var.labels="Reference to any Moral Foundation"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m3learn", no.space=T, table.placement="ht"
)


### Table for D8 [fig 4 + 2008]: engagement/sophistication X ideology -> specific mft reference (logit)

## 2012
stargazer(list(m4_know[[1]],m4_media[[1]],m4_disc[[1]],m4_all[[1]]
               , m4_know[[2]],m4_media[[2]],m4_disc[[2]],m4_all[[2]])
          , type="text", out="tab/m4ideolearn2012a.tex"
          , title="Logit models predicting references to specific moral foundations (2012)"
          , dep.var.labels = rev(gsub("\n","",mftLabs))[1:2], model.numbers = FALSE
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m4ideolearn2012a", no.space=T, table.placement="ht", order=c(1:5,12:17,6:11)
          , covariate.labels = c("Moderate","Conservative","Political Knowledge"
                                 , "Political Media Exposure","Political Discussion"
                                 , "Moderate X Knowledge","Conservative X Knowledge"
                                 , "Moderate X Media Exposure","Conservative X Media Exposure"
                                 , "Moderate X Discussion","Conservative X Discussion"
                                 , covLabs)
)
stargazer(list(m4_know[[3]],m4_media[[3]],m4_disc[[3]],m4_all[[3]]
               , m4_know[[4]],m4_media[[4]],m4_disc[[4]],m4_all[[4]])
          , type="text", out="tab/m4ideolearn2012b.tex"
          , title="Logit models predicting references to specific moral foundations (2012)"
          , dep.var.labels = rev(gsub("\n","",mftLabs))[3:4], model.numbers = FALSE
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m4ideolearn2012b", no.space=T, table.placement="ht", order=c(1:5,12:17,6:11)
          , covariate.labels = c("Moderate","Conservative","Political Knowledge"
                                 , "Political Media Exposure","Political Discussion"
                                 , "Moderate X Knowledge","Conservative X Knowledge"
                                 , "Moderate X Media Exposure","Conservative X Media Exposure"
                                 , "Moderate X Discussion","Conservative X Discussion"
                                 , covLabs)
)

## 2008
stargazer(list(m4_2008_know[[1]],m4_2008_media[[1]],m4_2008_disc[[1]],m4_2008_all[[1]]
               , m4_2008_know[[2]],m4_2008_media[[2]],m4_2008_disc[[2]],m4_2008_all[[2]])
          , type="text", out="tab/m4ideolearn2008a.tex"
          , title="Logit models predicting references to specific moral foundations (2008)"
          , dep.var.labels = rev(gsub("\n","",mftLabs))[1:2], model.numbers = FALSE
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m4ideolearn2008a", no.space=T, table.placement="ht", order=c(1:5,12:17,6:11)
          , covariate.labels = c("Moderate","Conservative","Political Knowledge"
                                 , "Political Media Exposure","Political Discussion"
                                 , "Moderate X Knowledge","Conservative X Knowledge"
                                 , "Moderate X Media Exposure","Conservative X Media Exposure"
                                 , "Moderate X Discussion","Conservative X Discussion"
                                 , covLabs)
)
stargazer(list(m4_2008_know[[3]],m4_2008_media[[3]],m4_2008_disc[[3]],m4_2008_all[[3]]
               , m4_2008_know[[4]],m4_2008_media[[4]],m4_2008_disc[[4]],m4_2008_all[[4]])
          , type="text", out="tab/m4ideolearn2008b.tex"
          , title="Logit models predicting references to specific moral foundations (2008)"
          , dep.var.labels = rev(gsub("\n","",mftLabs))[3:4], model.numbers = FALSE
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m4ideolearn2008b", no.space=T, table.placement="ht", order=c(1:5,12:17,6:11)
          , covariate.labels = c("Moderate","Conservative","Political Knowledge"
                                 , "Political Media Exposure","Political Discussion"
                                 , "Moderate X Knowledge","Conservative X Knowledge"
                                 , "Moderate X Media Exposure","Conservative X Media Exposure"
                                 , "Moderate X Discussion","Conservative X Discussion"
                                 , covLabs)
)



#######################################################################################
### Part 3: Investigating the political relevance of political reasoning including 2008


### Table for D9 [fig 5 + 2008]: mft -> turnout (logit)

stargazer(m5,m5_2008
          , type="text", out="tab/m5turnout.tex"
          , title="Logit models predicting turnout based on moral foundations"
          , covariate.labels=c(rev(gsub("\n","",mftLabs))
                               , "Strength of Party Identification", covLabs)
          , column.labels = c("2012","2008"), column.separate = c(2,2)
          , model.numbers = TRUE, dep.var.labels="Turnout"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m5turnout", no.space=T, table.placement="ht"
)


### Table for D11 [fig 7 + 2008]: mft -> feeling thermometer differentials (ols)

stargazer(m7[[1]],m7[[2]],m7_2008[[1]],m7_2008[[2]]
          ,m7[[3]],m7[[4]],m7_2008[[3]],m7_2008[[4]]
          , type="text", out="tab/m7feel.tex"
          , title="OLS models predicting feeling thermometer differentials"
          , dep.var.labels = c("Party Evaluations", "Candidate Evaluations")
          , covariate.labels=c(rev(gsub("\n","",mftLabs))
                               , "Party Identification (Democrats)"
                               , "Party Identification (Republicans)", covLabs)
          , column.labels = rep(c("2012","2008"),2), column.separate = c(2,2,2,2)
          , model.numbers = TRUE, keep.stat = c("n","rsq")
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m7feel", no.space=T, table.placement="ht"
)


### Table for D12 [fig 8 + 2008]: mft -> vote democratic (logit)

stargazer(m8,m8_2008
          , type="text", out="tab/m8vote.tex"
          , title="Logit models predicting Democratic vote choice based on moral foundations"
          , covariate.labels=c(rev(gsub("\n","",mftLabs))
                               , "Party Identification (Democrats)"
                               , "Party Identification (Republicans)", covLabs)
          , column.labels = c("2012","2008"), column.separate = c(2,2)
          , model.numbers = FALSE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m8vote", no.space=T, table.placement="ht"
)


