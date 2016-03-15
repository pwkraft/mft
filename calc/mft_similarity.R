###############################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     mft_analyses.R
## Overview: new analysis focusing on cosine similarity b/w dictionaries and responses
##           uses the datasets generated in mft_prep
## Author:   Patrick Kraft
###############################################################################################


rm(list=ls())
setwd("/data/Uni/projects/2014/mft/calc")

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
anes2012 <- mutate(anes2012, harm_s = scale(harm), fairness_s = scale(fairness)
                   , ingroup_s = scale(ingroup), authority_s = scale(authority)
                   , purity_s = scale(purity), general_s = scale(general))
anes2008 <- mutate(anes2008, harm_s = scale(harm), fairness_s = scale(fairness)
                   , ingroup_s = scale(ingroup), authority_s = scale(authority)
                   , purity_s = scale(purity), general_s = scale(general))




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


### Figure 2: ideology -> mft (logit)

## model estimation (SUR)
eqSystem <- list(harm = harm_s ~ ideol + relig + educ + age + female + black + num_total
               , fairness = fairness_s ~ ideol + relig + educ + age + female + black + num_total
               , ingroup = ingroup_s ~ ideol + relig + educ + age + female + black + num_total
               , authority = authority_s ~ ideol + relig + educ + age + female + black + num_total)
m2 <- systemfit(eqSystem, "SUR", data = anes2012)
m2res <- coef(summary(m2))
m2res <- data.frame(m2res[grep("Conservative",rownames(m2res)),1:2])
colnames(m2res) <- c("mean","se")
m2res$var <- 4:1
m2res$year <- "2012"

## generate plot
ggplot(m2res, aes(x = mean, y = var)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) + xlim(-0.4,0.4) + 
  geom_errorbarh(aes(xmax=mean+1.96*se,xmin=mean-1.96*se),height=.2) + 
  labs(y = "Dependent Variable:\nMoral Foundation"
       , x = "Change in Emphasis (in standard deviations)") + 
  theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
  ggtitle("Change in Emphasis on each Moral Foundation\nfor Conservatives compared to Liberals")  + 
  ggsave(filename = "fig/fig2ideol.pdf", width = 6, height = 4)



###########################################
### Part 2: Determinants of moral reasoning


### Figure 3: engagement -> general mft reference (logit)

## model estimation
m3 <- NULL
m3[[1]] <- lm(general_s ~ polknow + relig + educ + age + female + black + num_total, data=anes2012)
m3[[2]] <- lm(general_s ~ polmedia + relig + educ + age + female + black + num_total, data=anes2012)
m3[[3]] <- lm(general_s ~ poldisc + relig + educ + age + female + black + num_total, data=anes2012)
m3[[4]] <- lm(general_s ~ polknow + polmedia + poldisc
               + relig + educ + age + female + black + num_total
               , data=anes2012)

## simulation of predicted probabilities / first differences
m3_res <- rbind(sim(m3[[1]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[2]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[3]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(polknow=range(anes2012$polknow, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(polmedia=range(anes2012$polmedia, na.rm = T)))
                , sim(m3[[4]], iv=data.frame(poldisc=range(anes2012$poldisc, na.rm = T))))
m3_res$cond <- rep(c("No", "Yes"), each=3)
m3_res$var <- rep(3:1,2)
m3_res$year <- "2012"

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
  geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
  geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
  labs(y = "Independent Variable", x= "Change in Emphasis") + 
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



########################################################################
### Part 3: Investigating the political relevance of political reasoning


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
m5_res <- rbind(sim(m5, iv=data.frame(harm_s = range(anes2012$harm_s)))
                , sim(m5, iv=data.frame(fairness_s = range(anes2012$fairness_s)))
                , sim(m5, iv=data.frame(ingroup_s = range(anes2012$ingroup_s)))
                , sim(m5, iv=data.frame(authority_s = range(anes2012$authority_s))))
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
m6_res <- rbind(sim(m6, iv=data.frame(harm_s = range(anes2012$harm_s)), robust=T)
                , sim(m6, iv=data.frame(fairness_s = range(anes2012$fairness_s)), robust=T)
                , sim(m6, iv=data.frame(ingroup_s = range(anes2012$ingroup_s)), robust=T)
                , sim(m6, iv=data.frame(authority_s = range(anes2012$authority_s)), robust=T))
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
m7_res <- rbind(sim(m7, iv=data.frame(harm_s = range(anes2012$harm_s)), robust=T)
                , sim(m7, iv=data.frame(fairness_s = range(anes2012$fairness_s)), robust=T)
                , sim(m7, iv=data.frame(ingroup_s = range(anes2012$ingroup_s)), robust=T)
                , sim(m7, iv=data.frame(authority_s = range(anes2012$authority_s)), robust=T))
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
m8_res <- rbind(sim(m8, iv=data.frame(harm_s = range(anes2012$harm_s)))
                , sim(m8, iv=data.frame(fairness_s = range(anes2012$fairness_s)))
                , sim(m8, iv=data.frame(ingroup_s = range(anes2012$ingroup_s)))
                , sim(m8, iv=data.frame(authority_s = range(anes2012$authority_s))))
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


###########################################################################


### Robustness checks

## try tobit model shows same results
## could also use the censReg or AER packages...
t1 <- list(NULL)
t1[[1]] <- vglm(harm ~ ideol + relig + educ + age + female + black + num_total, tobit(Lower = 0)
                , data = data_tfidf)
t1[[2]] <- vglm(fairness ~ ideol + relig + educ + age + female + black + num_total, tobit(Lower = 0)
                , data = data_tfidf)
t1[[3]] <- vglm(authority ~ ideol + relig + educ + age + female + black + num_total, tobit(Lower = 0)
                , data = data_tfifd)
t1[[4]] <- vglm(ingroup ~ ideol + relig + educ + age + female + black + num_total, tobit(Lower = 0)
                , data = data_tfidf)
lapply(t1, summary)


## alternative model: dirichlet
m1raw <- data_dfm %>% dplyr::select(harm, fairness, authority, ingroup, purity
                                   , ideol_lib, ideol_con, relig, educ, age, female, black
                                  , num_total) %>% na.omit()
## model with Y matrix:Y = m1raw[,c("harm", "fairness", "authority", "ingroup", "purity")]
m1dl <- list(Y = m1raw[,"authority"] + .001
           , X = m1raw[,c("ideol_lib", "ideol_con", "relig", "educ", "age", "female", "black"
                        , "num_total")], I = nrow(m1raw), J = 8
           , X_new = data.frame(ideol_lib = c(1,0), ideol_con = c(0,1), relig = mean(m1raw$relig)
                              , educ = mean(m1raw$educ), age = mean(m1raw$educ)
                              , female = mean(m1raw$female), black = mean(m1raw$black)
                              , num_total = mean(m1raw$num_total)))
m1stan <- stan(file = "func/mft_beta.stan", data = m1dl)
print(m1stan, pars="gamma")


### regression discontinuity design investigating the effect of previous voting

# plot discontinuity for aggregate data
ggplot(data_tfidf, aes(x=regdi_year, y=general, color=regdi_year>=18, shape = regdi_year>=18)) + 
  geom_point() + scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2012 Survey)") +
  geom_smooth(method=lm) + theme_bw()

ggplot(data_dfm, aes(x=regdi_year, y=general, color=regdi_year>=18, shape = regdi_year>=18)) + 
  geom_point() + scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2012 Survey)") +
  geom_smooth(method=lm) + theme_bw()