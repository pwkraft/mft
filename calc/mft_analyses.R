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
        , mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
        , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology"
        , file = "fig/p1_mft_ideol.pdf", width = 6, height = 4)


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
m1_res$var <- 4:1
m1_res$year <- "2012"

## generate plot
ggplot(m1_res, aes(x = mean, y = var)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Dependent Variable:\nMoral Foundation"
     , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
    ggtitle("Change in Predicted Probabilities to\nReference each Moral Foundation")  + 
ggsave(filename = "fig/m1_mft.pdf", width = 6, height = 4)



###########################################
### Part 2: Determinants of moral reasoning


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
m3_res$year <- "2012"

## generate plot
ggplot(m3_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Independent Variable", x= "Change in Probability") + 
    theme_bw() + scale_y_continuous(breaks=3:1, labels=polLabs) +
    ggtitle("Change in Predicted Probabilities to Reference\nany Moral Foundation") +
    guides(lty=guide_legend(title="Control for both remaining variables")) +
    theme(legend.position="bottom", legend.box="horizontal")
ggsave(filename = "fig/m3_learn.pdf", width = 6, height = 4)


### Figure 4: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m3b_know <- NULL
m3b_know[[1]] <- glm(harm_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_know[[2]] <- glm(fair_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_know[[3]] <- glm(ingr_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_know[[4]] <- glm(auth_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_media <- NULL
m3b_media[[1]] <- glm(harm_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_media[[2]] <- glm(fair_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_media[[3]] <- glm(ingr_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_media[[4]] <- glm(auth_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2012, family=binomial("logit"))
m3b_disc <- NULL
m3b_disc[[1]] <- glm(harm_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_disc[[2]] <- glm(fair_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_disc[[3]] <- glm(ingr_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_disc[[4]] <- glm(auth_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m3b_all <- NULL
m3b_all[[1]] <- glm(harm_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m3b_all[[2]] <- glm(fair_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m3b_all[[3]] <- glm(ingr_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m3b_all[[4]] <- glm(auth_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
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
m3b_res$year <- "2012"
levels(m3b_res$dv) <- gsub("\n", "", rev(mftLabs))

## generate plot
ggplot(m3b_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
    theme_bw() + scale_y_continuous(breaks=3:1, labels=polLabs) +
    ggtitle("Change in Effect of Ideology on the\nProbability to Reference each Moral Foundation") +
    guides(lty=guide_legend(title="Control for Both Remaining Variables")) +
    theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv)
ggsave(filename = "fig/m3b_learn.pdf", width = 6, height = 5)



########################################################################
### Part 3: Investigating the political relevance of political reasoning


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
m2b_res$year <- "2012"

## generate plot
ggplot(m2b_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Probability") +
    theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
    ggtitle("Change in Predicted Probabilities to\nParticipate in Election") +
    guides(lty=guide_legend(title="Control for PID Strength")) +
    theme(legend.position="bottom", legend.box="horizontal")
ggsave(filename = "fig/m2b_vote.pdf", width = 6, height = 4)


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
m2e_res$year <- "2012"

## generate plot
ggplot(m2e_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Protest Index") +
    theme_bw() + ggtitle("Change in Protest Behavior Index") +
    guides(lty=guide_legend(title="Control for PID Strength")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/m2e_part.pdf", width = 6, height = 4)


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
m2f_res$year <- "2012"
levels(m2f_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(m2f_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Independent Variable: Moral Foundation"
       , x= "Change in Feeling Thermometer (Democrat - Republican)") +
    theme_bw() + ggtitle("Change in Feeling Thermometer Differentials") +
    guides(lty=guide_legend(title="Control for Party Identification")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs) + facet_grid(dv ~.)
ggsave(filename = "fig/m2f_vote.pdf", width = 6, height = 5)


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
m2_res$year <- "2012"

## generate plot
ggplot(m2_res, aes(x = mean, y = var+.1-.2*(cond=="Yes"), lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Probability") +
    theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
    ggtitle("Change in Predicted Probabilities to Vote\nfor Democratic Candidate") +
    guides(lty=guide_legend(title="Control for Party Identification")) +
    theme(legend.position="bottom", legend.box="horizontal")
ggsave(filename = "fig/m2_vote.pdf", width = 6, height = 4)




#############################
### Material for Appendix ###
#############################



#############################
### Appendix B: Data Overview


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
           , label="tab:a1_mis"),file="tab/appB_mis.tex")


### plot number of words (note that some max values are omitted)

## create individual plots
appB1 <- qplot(num_total, data=anes2008[anes2008$num_total>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2008)")
appB2 <- qplot(num_total, data=anes2012[anes2012$num_total>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2012)")
appB3 <- qplot(num_ca, data=anes2008[anes2008$num_ca>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2008)")
appB4 <- qplot(num_ca, data=anes2012[anes2012$num_ca>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2012)")
appB5 <- qplot(num_pa, data=anes2008[anes2008$num_pa>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2008)")
appB6 <- qplot(num_pa, data=anes2012[anes2012$num_pa>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2012)")

## save multiplot
pdf("fig/appB_num.pdf", width = 7, height = 5)
multiplot(appB1, appB2, appB3, appB4, appB5, appB6, cols=2)
dev.off()



############################################
### Appendix C: Additional descriptive plots


### proportion of respondents mentioning each moral foundation

## generate individual plots
appC1 <- prop_plot(data=list(anes2008,anes2012), groupvarname="ideol", legendname = "Ideology"
              , mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
              , title = "Moral Foundation and Ideology:\nAll Evaluations")
appC2 <- prop_plot(data=list(anes2008,anes2012), groupvarname="ideol", legendname = "Ideology"
              , mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
              , title = "Moral Foundation and Ideology:\nCandidate Evaluations")
appC3 <- prop_plot(data=list(anes2008,anes2012), groupvarname="ideol", legendname = "Ideology"
              , mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
              , title = "Moral Foundation and Ideology:\nParty Evaluations")
appC4 <- prop_plot(data=list(anes2008,anes2012)
              , groupvarname="pid", legendname = "Party Identification"
              , mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
              , title = "Moral Foundation and Party Identification:\nAll Evaluations")
appC5 <- prop_plot(data=list(anes2008,anes2012)
              , groupvarname="pid", legendname = "Party Identification"
              , mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
              , title = "Moral Foundation and Party Identification:\nCandidate Evaluations")
appC6 <- prop_plot(data=list(anes2008,anes2012)
              , groupvarname="pid", legendname = "Party Identification"
              , mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
              , title = "Moral Foundation and Party Identification:\nParty Evaluations")

## save multiplot
pdf("fig/appC.pdf", width = 12, height = 16)
multiplot(appC1, appC2, appC3, appC4, appC5, appC6, cols=2)
dev.off()



######################################################################
### Appendix D: Alternative model specifications and robustness checks


### Figure 2 [+ 2008]: ideology -> mft (logit)

## model estimation
m1_2008 <- NULL
m1_2008[[1]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m1_2008[[2]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m1_2008[[3]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m1_2008[[4]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m1_2008_res <- sim(m1_2008, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m1_2008_res$var <- 4:1
m1_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m1_res,m1_2008_res), aes(x = mean, y=var-.052+.11*(year=="2008")
                                    , shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable:\nMoral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to\nReference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/appD1.pdf")


### Figure 3 [+ 2008]: engagement -> general mft reference (logit)

## model estimation
m3_2008 <- NULL
m3_2008[[1]] <- glm(mft_all ~ polknow + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m3_2008[[2]] <- glm(mft_all ~ polmedia + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m3_2008[[3]] <- glm(mft_all ~ poldisc + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m3_2008[[4]] <- glm(mft_all ~ polknow + polmedia + poldisc
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m3_2008_res <- rbind(sim(m3_2008[[1]], iv=data.frame(polknow=range(anes2008$polknow, na.rm = T)))
                   , sim(m3_2008[[2]], iv=data.frame(polmedia=range(anes2008$polmedia, na.rm = T)))
                   , sim(m3_2008[[3]], iv=data.frame(poldisc=range(anes2008$poldisc, na.rm = T)))
                   , sim(m3_2008[[4]], iv=data.frame(polknow=range(anes2008$polknow, na.rm = T)))
                   , sim(m3_2008[[4]], iv=data.frame(polmedia=range(anes2008$polmedia, na.rm = T)))
                   , sim(m3_2008[[4]], iv=data.frame(poldisc=range(anes2008$poldisc, na.rm = T))))
m3_2008_res$cond <- rep(c("No", "Yes"), each=3)
m3_2008_res$var <- rep(3:1,2)
m3_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m3_res,m3_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                    , shape=year, color = year, lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable", x= "Change in Probability") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference\nany Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for both remaining variables")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=3:1, labels=polLabs)
ggsave(filename = "fig/appD2.pdf")



### Figure 4 [+ 2008]: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m3b_2008_know <- NULL
m3b_2008_know[[1]] <- glm(harm_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_know[[2]] <- glm(fair_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_know[[3]] <- glm(ingr_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_know[[4]] <- glm(auth_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_media <- NULL
m3b_2008_media[[1]] <- glm(harm_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m3b_2008_media[[2]] <- glm(fair_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m3b_2008_media[[3]] <- glm(ingr_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m3b_2008_media[[4]] <- glm(auth_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m3b_2008_disc <- NULL
m3b_2008_disc[[1]] <- glm(harm_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_disc[[2]] <- glm(fair_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_disc[[3]] <- glm(ingr_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_disc[[4]] <- glm(auth_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m3b_2008_all <- NULL
m3b_2008_all[[1]] <- glm(harm_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m3b_2008_all[[2]] <- glm(fair_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m3b_2008_all[[3]] <- glm(ingr_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m3b_2008_all[[4]] <- glm(auth_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))


## simulation of predicted probabilities / difference-in-difference
m3b_2008_res <- rbind(sim(models = m3b_2008_know
                   , iv=data.frame(polknow_c=rep(range(anes2008$polknow_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_2008_media
                   , iv=data.frame(polmedia_c=rep(range(anes2008$polmedia_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_2008_disc
                   , iv=data.frame(poldisc_c=rep(range(anes2008$poldisc_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_2008_all
                   , iv=data.frame(polknow_c=rep(range(anes2008$polknow_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_2008_all
                   , iv=data.frame(polmedia_c=rep(range(anes2008$polmedia_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1)))
               , sim(models = m3b_2008_all
                   , iv=data.frame(poldisc_c=rep(range(anes2008$poldisc_c, na.rm = T),2)
                                 , ideolModerate = rep(0,4)
                                 , ideolConservative = c(0,0,1,1))))
m3b_2008_res$var <- rep(rep(3:1,each=4),2)
m3b_2008_res$cond <- rep(c("No","Yes"), each = 12)
m3b_2008_res$year <- "2008"
levels(m3b_2008_res$dv) <- gsub("\n", "", rev(mftLabs))

## generate plot
ggplot(rbind(m3b_res,m3b_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                      , shape=year, color = year, lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") +
    theme_bw() + scale_y_continuous(breaks=3:1, labels=polLabs) +
    ggtitle("Change in Effect of Ideology on the\nProbability to Reference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for Both Remaining Variables")) +
    theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv) +
    scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD3.pdf")


### Figure 5 [+ 2008]: mft -> turnout (logit)

## model estimation
m2b_2008 <- NULL
m2b_2008[[1]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                     + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m2b_2008[[2]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                     + pid_str + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m2b_2008_res <- rbind(sim(m2b_2008, iv=data.frame(harm_all = c(0,1)))
               , sim(m2b_2008, iv=data.frame(fair_all = c(0,1)))
               , sim(m2b_2008, iv=data.frame(ingr_all = c(0,1)))
               , sim(m2b_2008, iv=data.frame(auth_all = c(0,1))))
m2b_2008_res$cond <- rep(c("No","Yes"),4)
m2b_2008_res$var <- rep(4:1,each=2)
m2b_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m2b_res,m2b_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
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
ggsave(filename = "fig/appD4.pdf")


### Figure 6 [+ 2008]: mft -> protest behavior index (ols)

## model estimation
m2e_2008 <- NULL
m2e_2008[[1]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black + num_total, data=anes2008)
m2e_2008[[2]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_str + relig + educ + age + female + black + num_total, data=anes2008)

## simulation of predicted probabilities / first differences
m2e_2008_res <- rbind(sim(m2e_2008, iv=data.frame(harm_all = c(0,1)), robust=T)
               , sim(m2e_2008, iv=data.frame(fair_all = c(0,1)), robust=T)
               , sim(m2e_2008, iv=data.frame(ingr_all = c(0,1)), robust=T)
               , sim(m2e_2008, iv=data.frame(auth_all = c(0,1)), robust=T))
m2e_2008_res$cond <- rep(c("No","Yes"),4)
m2e_2008_res$var <- rep(4:1,each=2)
m2e_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m2e_res,m2e_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
                                      , shape=year, color = year, lty=cond)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Independent Variable:\nMoral Foundation", x= "Change in Protest Index") +
    theme_bw() + ggtitle("Change in Protest Behavior Index") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")
         , lty=guide_legend(title="Control for PID Strength")) +
    theme(legend.position="bottom", legend.box="horizontal") + 
    scale_y_continuous(breaks=1:4, labels=mftLabs) +
    scale_color_manual(values=c("royalblue", "firebrick"))
ggsave(filename = "fig/appD5.pdf")


### Figure 7: mft -> feeling thermometer differentials (ols)

## model estimation
m2f_2008 <- NULL
m2f_2008[[1]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black, data=anes2008)
m2f_2008[[2]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008)
m2f_2008[[3]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black, data=anes2008)
m2f_2008[[4]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008)

## simulation of predicted probabilities / first differences
m2f_2008_res <- rbind(sim(m2f_2008, iv=data.frame(harm_all = c(0,1)), robust=T)
                    , sim(m2f_2008, iv=data.frame(fair_all = c(0,1)), robust=T)
                    , sim(m2f_2008, iv=data.frame(ingr_all = c(0,1)), robust=T)
                    , sim(m2f_2008, iv=data.frame(auth_all = c(0,1)), robust=T))
m2f_2008_res$cond <- rep(c("No","Yes"),8)
m2f_2008_res$var <- rep(4:1,each=4)
m2f_2008_res$year <- "2008"
levels(m2f_2008_res$dv) <- c("Party Evaluation", "Candidate Evaluation")

## generate plot
ggplot(rbind(m2f_res, m2f_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
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
ggsave(filename = "fig/appD6.pdf")


### Figure 8: mft -> vote democratic (logit)

## model estimation
m2_2008 <- NULL
m2_2008[[1]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black
                  , data=anes2008, family = binomial("logit"))
m2_2008[[2]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_dem + pid_rep + relig + educ + age + female + black
                  , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m2_2008_res <- rbind(sim(m2_2008, iv=data.frame(harm_all = c(0,1)))
                   , sim(m2_2008, iv=data.frame(fair_all = c(0,1)))
                   , sim(m2_2008, iv=data.frame(ingr_all = c(0,1)))
                   , sim(m2_2008, iv=data.frame(auth_all = c(0,1))))
m2_2008_res$cond <- rep(c("No","Yes"),4)
m2_2008_res$var <- rep(4:1,each=2)
m2_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m2_res,m2_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
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
ggsave(filename = "fig/appD7.pdf")


### Figure D8: social/economic ideology -> mft (logit)

## combine issue positions to dimension (mean); high values -> more liberal position
anes2008$ideol_econ <- with(anes2008, (issue_aid+issue_govspend-issue_medins+1)/3)
anes2008$ideol_social <- anes2008$issue_gay
anes2012$ideol_econ <- with(anes2012, (issue_aid+issue_govspend-issue_medins-issue_jobs+2)/4)
anes2012$ideol_social <- with(anes2012, (issue_gay+issue_abort)/2)

## check factor analysis for 2012
factanal(na.omit(anes2012[,grep("issue",names(anes2012))]), 1, rotation="varimax")
factanal(na.omit(anes2012[,grep("issue",names(anes2012))]), 2, rotation="varimax")

## model estimation
m1b <- NULL
m1b[[1]] <- glm(harm_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1b[[2]] <- glm(fair_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1b[[3]] <- glm(ingr_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1b[[4]] <- glm(auth_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1b[[5]] <- glm(harm_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m1b[[6]] <- glm(fair_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m1b[[7]] <- glm(ingr_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m1b[[8]] <- glm(auth_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m1b_res <- rbind(sim(m1b, iv=data.frame(ideol_econ=c(0,1)))
               , sim(m1b, iv=data.frame(ideol_social=c(0,1))))
m1b_res$var <- rep(4:1,4)
m1b_res$year <- rep(rep(c("2012","2008"),each = 4),2)
m1b_res$Ideology <- rep(c("Economic Dimension","Social Dimension"),each = 8)

## generate plot
ggplot(m1b_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable: Moral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + scale_y_continuous(breaks=1:4, labels=mftLabs) +
    facet_grid(Ideology ~ .)
ggsave(filename = "fig/appD8.pdf")


### Figure D9 [fig 2, no leader]: ideology -> mft (logit)

## model estimation
m1noleader <- NULL
m1noleader[[1]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m1noleader[[2]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m1noleader[[3]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m1noleader[[4]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m1noleader[[5]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))
m1noleader[[6]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))
m1noleader[[7]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))
m1noleader[[8]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m1noleader_res <- sim(m1noleader, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m1noleader_res$var <- rep(4:1,2)
m1noleader_res$year <- rep(c("2012","2008"),each=4)

## generate plot
ggplot(m1noleader_res, aes(x = mean, y=var-.052+.11*(year=="2008")
                         , shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable:\nMoral Foundation (no leader)"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to\nReference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/appD9.pdf")


### Figure D10: ideology -> mft (virtues/vices)

## model estimation
m1c <- NULL
m1c[[1]] <- glm(harm_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[2]] <- glm(harm_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[3]] <- glm(fair_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[4]] <- glm(fair_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[5]] <- glm(ingr_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[6]] <- glm(ingr_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[7]] <- glm(auth_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[8]] <- glm(auth_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1c[[9]] <- glm(harm_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m1c[[10]] <- glm(harm_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1c[[11]] <- glm(fair_virtue_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1c[[12]] <- glm(fair_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1c[[13]] <- glm(ingr_virtue_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1c[[14]] <- glm(ingr_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1c[[15]] <- glm(auth_virtue_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1c[[16]] <- glm(auth_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m1c_res <- sim(m1c, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m1c_res$var <- rep(8:1,2)
m1c_res$year <- rep(c("2012","2008"),each=8)

## generate plot
ggplot(m1c_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable: Moral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(by valence)") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + 
    scale_y_continuous(breaks=1:8, labels=c("Authority\n(vice)","Authority\n(virtue)"
                                          , "Ingroup\n(vice)","Ingroup\n(virtue)"
                                          , "Fairness\n(vice)", "Fairness\n(virtue)"
                                          , "Harm\n(vice)", "Harm\n(virtue)"))
ggsave(filename = "fig/appD10.pdf")


### Figure D11: ideology -> mft (in-/out-party)

## in-party DV
anes2012$harm_in <- anes2012$harm_dem
anes2012$harm_in[which(anes2012$vote_dem==0)] <- anes2012$harm_rep[which(anes2012$vote_dem==0)]
anes2012$fair_in <- anes2012$fair_dem
anes2012$fair_in[which(anes2012$vote_dem==0)] <- anes2012$fair_rep[which(anes2012$vote_dem==0)]
anes2012$ingr_in <- anes2012$ingr_dem
anes2012$ingr_in[which(anes2012$vote_dem==0)] <- anes2012$ingr_rep[which(anes2012$vote_dem==0)]
anes2012$auth_in <- anes2012$auth_dem
anes2012$auth_in[which(anes2012$vote_dem==0)] <- anes2012$auth_rep[which(anes2012$vote_dem==0)]
anes2008$harm_in <- anes2008$harm_dem
anes2008$harm_in[which(anes2008$vote_dem==0)] <- anes2008$harm_rep[which(anes2008$vote_dem==0)]
anes2008$fair_in <- anes2008$fair_dem
anes2008$fair_in[which(anes2008$vote_dem==0)] <- anes2008$fair_rep[which(anes2008$vote_dem==0)]
anes2008$ingr_in <- anes2008$ingr_dem
anes2008$ingr_in[which(anes2008$vote_dem==0)] <- anes2008$ingr_rep[which(anes2008$vote_dem==0)]
anes2008$auth_in <- anes2008$auth_dem
anes2008$auth_in[which(anes2008$vote_dem==0)] <- anes2008$auth_rep[which(anes2008$vote_dem==0)]

## out-party DV
anes2012$harm_out <- anes2012$harm_dem
anes2012$harm_out[which(anes2012$vote_dem==1)] <- anes2012$harm_rep[which(anes2012$vote_dem==1)]
anes2012$fair_out <- anes2012$fair_dem
anes2012$fair_out[which(anes2012$vote_dem==1)] <- anes2012$fair_rep[which(anes2012$vote_dem==1)]
anes2012$ingr_out <- anes2012$ingr_dem
anes2012$ingr_out[which(anes2012$vote_dem==1)] <- anes2012$ingr_rep[which(anes2012$vote_dem==1)]
anes2012$auth_out <- anes2012$auth_dem
anes2012$auth_out[which(anes2012$vote_dem==1)] <- anes2012$auth_rep[which(anes2012$vote_dem==1)]
anes2008$harm_out <- anes2008$harm_dem
anes2008$harm_out[which(anes2008$vote_dem==1)] <- anes2008$harm_rep[which(anes2008$vote_dem==1)]
anes2008$fair_out <- anes2008$fair_dem
anes2008$fair_out[which(anes2008$vote_dem==1)] <- anes2008$fair_rep[which(anes2008$vote_dem==1)]
anes2008$ingr_out <- anes2008$ingr_dem
anes2008$ingr_out[which(anes2008$vote_dem==1)] <- anes2008$ingr_rep[which(anes2008$vote_dem==1)]
anes2008$auth_out <- anes2008$auth_dem
anes2008$auth_out[which(anes2008$vote_dem==1)] <- anes2008$auth_rep[which(anes2008$vote_dem==1)]

## model estimation
m1d <- NULL
m1d[[1]] <- glm(harm_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[2]] <- glm(harm_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[3]] <- glm(fair_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[4]] <- glm(fair_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[5]] <- glm(ingr_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[6]] <- glm(ingr_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[7]] <- glm(auth_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[8]] <- glm(auth_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1d[[9]] <- glm(harm_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m1d[[10]] <- glm(harm_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1d[[11]] <- glm(fair_in ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1d[[12]] <- glm(fair_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1d[[13]] <- glm(ingr_in ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1d[[14]] <- glm(ingr_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1d[[15]] <- glm(auth_in ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1d[[16]] <- glm(auth_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m1d_res <- sim(m1d, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m1d_res$var <- rep(8:1,2)
m1d_res$year <- rep(c("2012","2008"),each=8)

## generate plot
ggplot(m1d_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable: Moral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(by in-party/out-party)") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + 
    scale_y_continuous(breaks=1:8, labels=c("Authority\n(out)","Authority\n(in)"
                                          , "Ingroup\n(out)","Ingroup\n(in)"
                                          , "Fairness\n(out)", "Fairness\n(in)"
                                          , "Harm\n(out)", "Harm\n(in)"))
ggsave(filename = "fig/appD11.pdf")


### Figure D12: ideology -> mft (likes/dislikes)

## model estimation
m1e <- NULL
m1e[[1]] <- glm(harm_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[2]] <- glm(harm_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[3]] <- glm(fair_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[4]] <- glm(fair_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[5]] <- glm(ingr_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[6]] <- glm(ingr_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[7]] <- glm(auth_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[8]] <- glm(auth_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m1e[[9]] <- glm(harm_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m1e[[10]] <- glm(harm_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1e[[11]] <- glm(fair_li ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1e[[12]] <- glm(fair_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1e[[13]] <- glm(ingr_li ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1e[[14]] <- glm(ingr_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1e[[15]] <- glm(auth_li ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m1e[[16]] <- glm(auth_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m1e_res <- sim(m1e, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m1e_res$var <- rep(8:1,2)
m1e_res$year <- rep(c("2012","2008"),each=8)

## generate plot
ggplot(m1e_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable: Moral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(by like/dislike)") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + 
    scale_y_continuous(breaks=1:8, labels=c("Authority\n(di)","Authority\n(li)"
                                          , "Ingroup\n(di)","Ingroup\n(li)"
                                          , "Fairness\n(di)", "Fairness\n(li)"
                                          , "Harm\n(di)", "Harm\n(li)"))
ggsave(filename = "fig/appD12.pdf")




#########################################
### Appendix E: Tables of model estimates


### Table for figure 2 [+ 2008]: ideology -> mft (logit)

stargazer(m1[[1]], m1_2008[[1]], m1[[2]], m1_2008[[2]]
        , m1[[3]], m1_2008[[3]], m1[[4]], m1_2008[[4]]
        , type="text", out="tab/m1_mft.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology"
        , covariate.labels = c("Conservative", "Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1_mft", no.space=T, table.placement="ht"
          )


### Table for figure 3 [+ 2008]: engagement -> general mft reference (logit)

stargazer(m3, m3_2008
        , type="text", out="tab/m3_learn.tex"
        , title="Logit Models Predicting Overall References to Moral Foundations"
        , covariate.labels = c("Political Knowledge","Political Media Exposure"
                              ,"Political Discussion",covLabs)
        , column.labels = c("2012","2008"), column.separate = c(4,4)
        , model.numbers = FALSE
        , dep.var.labels="Reference to any Moral Foundation"
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m3_learn", no.space=T, table.placement="ht"
          )


### Table for figure 4 [+ 2008]: engagement/sophistication X ideology -> specific mft reference (logit)

## 2012
stargazer(list(m3b_know[[1]],m3b_media[[1]],m3b_disc[[1]],m3b_all[[1]]
             , m3b_know[[2]],m3b_media[[2]],m3b_disc[[2]],m3b_all[[2]]
             , m3b_know[[3]],m3b_media[[3]],m3b_disc[[3]],m3b_all[[3]]
             , m3b_know[[4]],m3b_media[[4]],m3b_disc[[4]],m3b_all[[4]])
        , type="text", out="tab/m3b2012.tex"
        , title="Logit Models Predicting References to Specific Moral Foundations (2012)"
        , dep.var.labels = rev(gsub("\n","",mftLabs)), model.numbers = FALSE
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m3b_learn", no.space=T, table.placement="ht", order=c(1:5,12:17,6:11)
        , covariate.labels = c("Moderate","Conservative","Political Knowledge"
                             , "Political Media Exposure","Political Discussion"
                             , "Moderate X Knowledge","Conservative X Knowledge"
                             , "Moderate X Media Exposure","Conservative X Media Exposure"
                             , "Moderate X Discussion","Conservative X Discussion"
                             , covLabs)
          )

## 2008
stargazer(list(m3b_2008_know[[1]],m3b_2008_media[[1]],m3b_2008_disc[[1]],m3b_2008_all[[1]]
             , m3b_2008_know[[2]],m3b_2008_media[[2]],m3b_2008_disc[[2]],m3b_2008_all[[2]]
             , m3b_2008_know[[3]],m3b_2008_media[[3]],m3b_2008_disc[[3]],m3b_2008_all[[3]]
             , m3b_2008_know[[4]],m3b_2008_media[[4]],m3b_2008_disc[[4]],m3b_2008_all[[4]])
        , type="text", out="tab/m3b2008.tex"
        , title="Logit Models Predicting References to Specific Moral Foundations (2008)"
        , dep.var.labels = rev(gsub("\n","",mftLabs)), model.numbers = FALSE
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m3b_learn", no.space=T, table.placement="ht", order=c(1:5,12:17,6:11)
        , covariate.labels = c("Moderate","Conservative","Political Knowledge"
                             , "Political Media Exposure","Political Discussion"
                             , "Moderate X Knowledge","Conservative X Knowledge"
                             , "Moderate X Media Exposure","Conservative X Media Exposure"
                             , "Moderate X Discussion","Conservative X Discussion"
                             , covLabs)
          )


### Figure 5 [+ 2008]: mft -> turnout (logit)

stargazer(m2b,m2b_2008
        , type="text", out="tab/m2b_vote.tex"
        , title="Logit Models Predicting Turnout Based on Moral Foundations"
        , covariate.labels=c(rev(gsub("\n","",mftLabs))
                           , "Strength of Party Identification", covLabs)
        , column.labels = c("2012","2008"), column.separate = c(2,2)
        , model.numbers = TRUE, dep.var.labels="Turnout"
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2b_vote", no.space=T, table.placement="ht"
          )


### Figure 6: mft -> protest behavior index (ols)

stargazer(m2e,m2e_2008
        , type="text", out="tab/m2e.tex"
        , title="Logit Models Predicting Wearing Button/Sign Based on Moral Foundations"
        , covariate.labels=c(rev(gsub("\n","",mftLabs))
                             , "Strength of Party Identification", covLabs)
        , column.labels = c("2012","2008"), column.separate = c(2,2)
        , model.numbers = TRUE, keep.stat = c("n","rsq")
        , dep.var.labels="Vote for Democratic Presidential Candidate"
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2e", no.space=T, table.placement="ht"
          )


### Figure 7: mft -> feeling thermometer differentials (ols)

stargazer(m2f[[1]],m2f[[2]],m2f_2008[[1]],m2f_2008[[2]]
         ,m2f[[3]],m2f[[4]],m2f_2008[[3]],m2f_2008[[4]]
        , type="text", out="tab/m2f.tex"
        , title="Linear Model Predicting Feeling Thermometer Differential"
        , dep.var.labels = c("Party Evaluations", "Candidate Evaluations")
        , covariate.labels=c(rev(gsub("\n","",mftLabs))
                           , "Party Identification (Democrats)"
                           , "Party Identification (Republicans)", covLabs)
        , column.labels = rep(c("2012","2008"),2), column.separate = c(2,2,2,2)
        , model.numbers = TRUE, keep.stat = c("n","rsq")
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2f", no.space=T, table.placement="ht"
          )


### Figure 8: mft -> vote democratic (logit)

stargazer(m2,m2_2008
        , type="text", out="tab/m2_vote.tex"
        , title="Logit Models Predicting Democratic Vote Choice Based on Moral Foundations"
        , covariate.labels=c(rev(gsub("\n","",mftLabs))
                           , "Party Identification (Democrats)"
                           , "Party Identification (Republicans)", covLabs)
        , column.labels = c("2012","2008"), column.separate = c(2,2)
        , model.numbers = FALSE
        , dep.var.labels="Vote for Democratic Presidential Candidate"
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2_vote", no.space=T, table.placement="ht"
          )


### Figure D8: social/economic ideology -> mft (logit)

stargazer(m1b[[1]], m1b[[5]], m1b[[2]], m1b[[6]], m1b[[3]], m1b[[7]], m1b[[4]], m1b[[8]]
        , type="text", out="tab/m1b_mft.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Two-Dimensional Conceptualization of Ideology"
        , covariate.labels = c("Economic Liberalism","Social Liberalism",covLabs)
        , column.labels = rep(c("2012","2008"),4), model.numbers = FALSE
        , dep.var.labels=rev(gsub("\n","",mftLabs))
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1b_mft", no.space=T, table.placement="ht"
)


### Figure D9 [fig 2, no leader]: ideology -> mft (logit)

stargazer(m1noleader[[1]], m1noleader[[5]], m1noleader[[2]], m1noleader[[6]]
        , m1noleader[[3]], m1noleader[[7]], m1noleader[[4]], m1noleader[[8]]
        , type="text", out="tab/m1noleader.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology"
        , covariate.labels = c("Conservative", "Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1noleader", no.space=T, table.placement="ht"
          )


### Figure D10: ideology -> mft (virtues/vices)

## virtues
stargazer(m1c[[1]], m1c[[9]], m1c[[3]], m1c[[11]], m1c[[5]], m1c[[13]], m1c[[7]], m1c[[15]]
        , type="text", out="tab/m1c_virtue.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology (virtues)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1_virtue", no.space=T, table.placement="ht"
          )

## vices
stargazer(m1c[[2]], m1c[[10]], m1c[[4]], m1c[[12]], m1c[[6]], m1c[[14]], m1c[[8]], m1c[[16]]
        , type="text", out="tab/m1c_vice.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology (vices)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1_vice", no.space=T, table.placement="ht"
          )


### Figure D11: ideology -> mft (in-/out-party)

## in-party
stargazer(m1d[[1]], m1d[[9]], m1d[[3]], m1d[[11]], m1d[[5]], m1d[[13]], m1d[[7]], m1d[[15]]
        , type="text", out="tab/m1d_inparty.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology (in-party)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1d_inparty", no.space=T, table.placement="ht"
          )

## out-party
stargazer(m1d[[2]], m1d[[10]], m1d[[4]], m1d[[12]], m1d[[6]], m1d[[14]], m1d[[8]], m1d[[16]]
        , type="text", out="tab/m1d_outparty.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology (out-party)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1d_outparty", no.space=T, table.placement="ht"
)


### Figure D12: ideology -> mft (likes/dislikes)

## likes
stargazer(m1e[[1]], m1e[[9]], m1e[[3]], m1e[[11]], m1e[[5]], m1e[[13]], m1e[[7]], m1e[[15]]
        , type="text", out="tab/m1e_likes.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology (likes)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate",covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1_likes", no.space=T, table.placement="ht"
          )

## dislikes
stargazer(m1e[[2]], m1e[[10]], m1e[[4]], m1e[[12]], m1e[[6]], m1e[[14]], m1e[[8]], m1e[[16]]
        , type="text", out="tab/m1e_dislikes.tex"
        , title="Logit Models Predicting References to four Moral Foundations using Ideology (dislikes)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate",covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m1_dislikes", no.space=T, table.placement="ht"
          )

