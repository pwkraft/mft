##########################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     mft_analyses.R
## Overview: main analyses, creates all plots and tables for paper + appendix;
##           uses the datasets generated in mft_prep
## Author:   Patrick Kraft
##########################################################################################


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
        , file = "fig/fig1prop.pdf", width = 6, height = 4)


### Figure 2: ideology -> mft (logit)

## model estimation
m2 <- NULL
m2[[1]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m2[[2]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m2[[3]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))
m2[[4]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
             , data=anes2012, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m2_res <- sim(m2, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m2_res$var <- 4:1
m2_res$year <- "2012"

## generate plot
ggplot(m2_res, aes(x = mean, y = var)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=3) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.2) + 
    labs(y = "Dependent Variable:\nMoral Foundation"
     , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_y_continuous(breaks=1:4, labels=mftLabs) +
    ggtitle("Change in Predicted Probabilities to\nReference each Moral Foundation")  + 
ggsave(filename = "fig/fig2ideol.pdf", width = 6, height = 4)



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
ggsave(filename = "fig/fig3learn.pdf", width = 6, height = 4)


### Figure 4: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m4_know <- NULL
m4_know[[1]] <- glm(harm_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_know[[2]] <- glm(fair_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_know[[3]] <- glm(ingr_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_know[[4]] <- glm(auth_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_media <- NULL
m4_media[[1]] <- glm(harm_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m4_media[[2]] <- glm(fair_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m4_media[[3]] <- glm(ingr_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m4_media[[4]] <- glm(auth_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                   , data=anes2012, family=binomial("logit"))
m4_disc <- NULL
m4_disc[[1]] <- glm(harm_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_disc[[2]] <- glm(fair_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_disc[[3]] <- glm(ingr_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_disc[[4]] <- glm(auth_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                  , data=anes2012, family=binomial("logit"))
m4_all <- NULL
m4_all[[1]] <- glm(harm_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                 , data=anes2012, family=binomial("logit"))
m4_all[[2]] <- glm(fair_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                 , data=anes2012, family=binomial("logit"))
m4_all[[3]] <- glm(ingr_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                 , data=anes2012, family=binomial("logit"))
m4_all[[4]] <- glm(auth_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                   + relig + educ + age + female + black + num_total
                 , data=anes2012, family=binomial("logit"))


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
    ggtitle("Change in Effect of Ideology on the\nProbability to Reference each Moral Foundation") +
    guides(lty=guide_legend(title="Control for Both Remaining Variables")) +
    theme(legend.position="bottom", legend.box="horizontal") + facet_wrap(~dv)
ggsave(filename = "fig/fig4ideolearn.pdf", width = 6, height = 5)



########################################################################
### Part 3: Investigating the political relevance of political reasoning


### Figure 5: mft -> turnout (logit)

## model estimation
m5 <- NULL
m5[[1]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                + relig + educ + age + female + black + num_total
              , data=anes2012, family=binomial("logit"))
m5[[2]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                + pid_str + relig + educ + age + female + black + num_total
              , data=anes2012, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m5_res <- rbind(sim(m5, iv=data.frame(harm_all = c(0,1)))
               , sim(m5, iv=data.frame(fair_all = c(0,1)))
               , sim(m5, iv=data.frame(ingr_all = c(0,1)))
               , sim(m5, iv=data.frame(auth_all = c(0,1))))
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
m6[[1]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black + num_total, data=anes2012)
m6[[2]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
               + pid_str + relig + educ + age + female + black + num_total, data=anes2012)

## simulation of predicted probabilities / first differences
m6_res <- rbind(sim(m6, iv=data.frame(harm_all = c(0,1)), robust=T)
               , sim(m6, iv=data.frame(fair_all = c(0,1)), robust=T)
               , sim(m6, iv=data.frame(ingr_all = c(0,1)), robust=T)
               , sim(m6, iv=data.frame(auth_all = c(0,1)), robust=T))
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
m7[[1]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black, data=anes2012)
m7[[2]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
               + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012)
m7[[3]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black, data=anes2012)
m7[[4]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
               + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012)

## simulation of predicted probabilities / first differences
m7_res <- rbind(sim(m7, iv=data.frame(harm_all = c(0,1)), robust=T)
               , sim(m7, iv=data.frame(fair_all = c(0,1)), robust=T)
               , sim(m7, iv=data.frame(ingr_all = c(0,1)), robust=T)
               , sim(m7, iv=data.frame(auth_all = c(0,1)), robust=T))
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
m8[[1]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
               + relig + educ + age + female + black
             , data=anes2012, family = binomial("logit"))
m8[[2]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
               + pid_dem + pid_rep + relig + educ + age + female + black
             , data=anes2012, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m8_res <- rbind(sim(m8, iv=data.frame(harm_all = c(0,1)))
               , sim(m8, iv=data.frame(fair_all = c(0,1)))
               , sim(m8, iv=data.frame(ingr_all = c(0,1)))
               , sim(m8, iv=data.frame(auth_all = c(0,1))))
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
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2012)")
appB2b <- qplot(num_ca, data=anes2012[anes2012$num_ca>0, ], geom="bar", binwidth = 1
              , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2012)")
appB2c <- qplot(num_pa, data=anes2012[anes2012$num_pa>0, ], geom="bar", binwidth = 1
              , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2012)")
appB2d <- qplot(num_total, data=anes2008[anes2008$num_total>0, ], geom="bar", binwidth = 1
              , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2008)")
appB2e <- qplot(num_ca, data=anes2008[anes2008$num_ca>0, ], geom="bar", binwidth = 1
              , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2008)")
appB2f <- qplot(num_pa, data=anes2008[anes2008$num_pa>0, ], geom="bar", binwidth = 1
              , ylab = "Frequency", xlab = "Number of Words") +
    theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2008)")


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


### Figure D1 [fig 2 + 2008]: ideology -> mft (logit)

## model estimation
m2_2008 <- NULL
m2_2008[[1]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m2_2008[[2]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m2_2008[[3]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m2_2008[[4]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m2_2008_res <- sim(m2_2008, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m2_2008_res$var <- 4:1
m2_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m2_res,m2_2008_res), aes(x = mean, y=var-.052+.11*(year=="2008")
                                    , shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable:\nMoral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to\nReference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/appD1ideol.pdf", height = 5)


### Figure D2: social/economic ideology -> mft (logit)

## combine issue positions to dimension (mean); high values -> more liberal position
anes2008$ideol_econ <- with(anes2008, (issue_aid+issue_govspend-issue_medins+1)/3)
anes2008$ideol_social <- anes2008$issue_gay
anes2012$ideol_econ <- with(anes2012, (issue_aid+issue_govspend-issue_medins-issue_jobs+2)/4)
anes2012$ideol_social <- with(anes2012, (issue_gay+issue_abort)/2)

## check factor analysis for 2012
factanal(na.omit(anes2012[,grep("issue",names(anes2012))]), 1, rotation="varimax")
factanal(na.omit(anes2012[,grep("issue",names(anes2012))]), 2, rotation="varimax")

## model estimation
m2soceco <- NULL
m2soceco[[1]] <- glm(harm_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2soceco[[2]] <- glm(fair_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2soceco[[3]] <- glm(ingr_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2soceco[[4]] <- glm(auth_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2soceco[[5]] <- glm(harm_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m2soceco[[6]] <- glm(fair_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m2soceco[[7]] <- glm(ingr_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m2soceco[[8]] <- glm(auth_all ~ ideol_econ + ideol_social
                + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m2soceco_res <- rbind(sim(m2soceco, iv=data.frame(ideol_econ=c(0,1)))
               , sim(m2soceco, iv=data.frame(ideol_social=c(0,1))))
m2soceco_res$var <- rep(4:1,4)
m2soceco_res$year <- rep(rep(c("2012","2008"),each = 4),2)
m2soceco_res$Ideology <- rep(c("Economic Dimension","Social Dimension"),each = 8)

## generate plot
ggplot(m2soceco_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable: Moral Foundation"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + scale_y_continuous(breaks=1:4, labels=mftLabs) +
    facet_grid(Ideology ~ .)
ggsave(filename = "fig/appD2soceco.pdf", height = 5)


### Figure D3 [fig 2, no leader]: ideology -> mft (logit)

## model estimation
m2lead <- NULL
m2lead[[1]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m2lead[[2]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m2lead[[3]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m2lead[[4]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2012noleader, family=binomial("logit"))
m2lead[[5]] <- glm(harm_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))
m2lead[[6]] <- glm(fair_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))
m2lead[[7]] <- glm(ingr_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))
m2lead[[8]] <- glm(auth_all ~ ideol + relig + educ + age + female + black + num_total
                     , data=anes2008noleader, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m2lead_res <- sim(m2lead, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m2lead_res$var <- rep(4:1,2)
m2lead_res$year <- rep(c("2012","2008"),each=4)

## generate plot
ggplot(m2lead_res, aes(x = mean, y=var-.052+.11*(year=="2008")
                         , shape=year, color = year)) +
    geom_vline(xintercept=0, col="grey") + geom_point(size=4) +
    geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
    labs(y = "Dependent Variable:\nMoral Foundation (no leader)"
       , x = "Conservatives more likey                             Liberals more likely") + 
    theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
    ggtitle("Change in Predicted Probabilities to\nReference each Moral Foundation") +
    guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
    theme(legend.position="bottom") + scale_y_continuous(breaks=1:4, labels=mftLabs)
ggsave(filename = "fig/appD3lead.pdf", height = 5)


### Figure D4: ideology -> mft (virtues/vices)

## model estimation
m2val <- NULL
m2val[[1]] <- glm(harm_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[2]] <- glm(harm_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[3]] <- glm(fair_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[4]] <- glm(fair_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[5]] <- glm(ingr_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[6]] <- glm(ingr_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[7]] <- glm(auth_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[8]] <- glm(auth_vice_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2val[[9]] <- glm(harm_virtue_all ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m2val[[10]] <- glm(harm_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2val[[11]] <- glm(fair_virtue_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2val[[12]] <- glm(fair_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2val[[13]] <- glm(ingr_virtue_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2val[[14]] <- glm(ingr_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2val[[15]] <- glm(auth_virtue_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2val[[16]] <- glm(auth_vice_all ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m2val_res <- sim(m2val, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m2val_res$var <- rep(8:1,2)
m2val_res$year <- rep(c("2012","2008"),each=8)

## generate plot
ggplot(m2val_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
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
ggsave(filename = "fig/appD4val.pdf", height = 6)


### Figure D5: ideology -> mft (in-/out-party)

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
m2inout <- NULL
m2inout[[1]] <- glm(harm_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[2]] <- glm(harm_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[3]] <- glm(fair_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[4]] <- glm(fair_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[5]] <- glm(ingr_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[6]] <- glm(ingr_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[7]] <- glm(auth_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[8]] <- glm(auth_out ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2inout[[9]] <- glm(harm_in ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m2inout[[10]] <- glm(harm_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2inout[[11]] <- glm(fair_in ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2inout[[12]] <- glm(fair_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2inout[[13]] <- glm(ingr_in ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2inout[[14]] <- glm(ingr_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2inout[[15]] <- glm(auth_in ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2inout[[16]] <- glm(auth_out ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m2inout_res <- sim(m2inout, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m2inout_res$var <- rep(8:1,2)
m2inout_res$year <- rep(c("2012","2008"),each=8)

## generate plot
ggplot(m2inout_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
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
ggsave(filename = "fig/appD5inout.pdf", height = 6)


### Figure D7: ideology -> mft (likes/dislikes)

## model estimation
m2lidi <- NULL
m2lidi[[1]] <- glm(harm_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[2]] <- glm(harm_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[3]] <- glm(fair_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[4]] <- glm(fair_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[5]] <- glm(ingr_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[6]] <- glm(ingr_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[7]] <- glm(auth_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[8]] <- glm(auth_di ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2012, family = binomial("logit"))
m2lidi[[9]] <- glm(harm_li ~ ideol + relig + educ + age + female + black + num_total
              , data=anes2008, family = binomial("logit"))
m2lidi[[10]] <- glm(harm_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2lidi[[11]] <- glm(fair_li ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2lidi[[12]] <- glm(fair_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2lidi[[13]] <- glm(ingr_li ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2lidi[[14]] <- glm(ingr_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2lidi[[15]] <- glm(auth_li ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))
m2lidi[[16]] <- glm(auth_di ~ ideol + relig + educ + age + female + black + num_total
               , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m2lidi_res <- sim(m2lidi, iv=data.frame(ideolModerate=c(0,0), ideolConservative=c(1,0)))
m2lidi_res$var <- rep(8:1,2)
m2lidi_res$year <- rep(c("2012","2008"),each=8)

## generate plot
ggplot(m2lidi_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
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
ggsave(filename = "fig/appD6lidi.pdf", height = 6)



##########################################################
### Part 2: Determinants of moral reasoning including 2008


### Figure D7 [fig 3 + 2008]: engagement -> general mft reference (logit)

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
ggsave(filename = "fig/appD7learn.pdf", height = 6)


### Figure D8 [fig 4 + 2008]: engagement/sophistication X ideology -> specific mft reference (logit)

## model estimation
m4_2008_know <- NULL
m4_2008_know[[1]] <- glm(harm_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_know[[2]] <- glm(fair_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_know[[3]] <- glm(ingr_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_know[[4]] <- glm(auth_all ~ ideol*polknow_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_media <- NULL
m4_2008_media[[1]] <- glm(harm_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m4_2008_media[[2]] <- glm(fair_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m4_2008_media[[3]] <- glm(ingr_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m4_2008_media[[4]] <- glm(auth_all ~ ideol*polmedia_c + relig + educ + age + female + black + num_total
                    , data=anes2008, family=binomial("logit"))
m4_2008_disc <- NULL
m4_2008_disc[[1]] <- glm(harm_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_disc[[2]] <- glm(fair_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_disc[[3]] <- glm(ingr_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_disc[[4]] <- glm(auth_all ~ ideol*poldisc_c + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m4_2008_all <- NULL
m4_2008_all[[1]] <- glm(harm_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m4_2008_all[[2]] <- glm(fair_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m4_2008_all[[3]] <- glm(ingr_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))
m4_2008_all[[4]] <- glm(auth_all ~ ideol*polknow_c + ideol*polmedia_c + ideol*poldisc_c
                    + relig + educ + age + female + black + num_total
                  , data=anes2008, family=binomial("logit"))


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
    ggtitle("Change in Effect of Ideology on the\nProbability to Reference each Moral Foundation") +
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
m5_2008[[1]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                     + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))
m5_2008[[2]] <- glm(vote ~ harm_all + fair_all + ingr_all + auth_all
                     + pid_str + relig + educ + age + female + black + num_total
                   , data=anes2008, family=binomial("logit"))

## simulation of predicted probabilities / first differences
m5_2008_res <- rbind(sim(m5_2008, iv=data.frame(harm_all = c(0,1)))
               , sim(m5_2008, iv=data.frame(fair_all = c(0,1)))
               , sim(m5_2008, iv=data.frame(ingr_all = c(0,1)))
               , sim(m5_2008, iv=data.frame(auth_all = c(0,1))))
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


### Figure D10 [fig 6 + 2008]: mft -> protest behavior index (ols)

## model estimation
m6_2008 <- NULL
m6_2008[[1]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black + num_total, data=anes2008)
m6_2008[[2]] <- lm(part ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_str + relig + educ + age + female + black + num_total, data=anes2008)

## simulation of predicted probabilities / first differences
m6_2008_res <- rbind(sim(m6_2008, iv=data.frame(harm_all = c(0,1)), robust=T)
               , sim(m6_2008, iv=data.frame(fair_all = c(0,1)), robust=T)
               , sim(m6_2008, iv=data.frame(ingr_all = c(0,1)), robust=T)
               , sim(m6_2008, iv=data.frame(auth_all = c(0,1)), robust=T))
m6_2008_res$cond <- rep(c("No","Yes"),4)
m6_2008_res$var <- rep(4:1,each=2)
m6_2008_res$year <- "2008"

## generate plot
ggplot(rbind(m6_res,m6_2008_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes")
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
ggsave(filename = "fig/appD10part.pdf", height = 6)


### Figure D11 [fig 7 + 2008]: mft -> feeling thermometer differentials (ols)

## model estimation
m7_2008 <- NULL
m7_2008[[1]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black, data=anes2008)
m7_2008[[2]] <- lm(eval_party ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008)
m7_2008[[3]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black, data=anes2008)
m7_2008[[4]] <- lm(eval_cand ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008)

## simulation of predicted probabilities / first differences
m7_2008_res <- rbind(sim(m7_2008, iv=data.frame(harm_all = c(0,1)), robust=T)
                    , sim(m7_2008, iv=data.frame(fair_all = c(0,1)), robust=T)
                    , sim(m7_2008, iv=data.frame(ingr_all = c(0,1)), robust=T)
                    , sim(m7_2008, iv=data.frame(auth_all = c(0,1)), robust=T))
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
m8_2008[[1]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
                    + relig + educ + age + female + black
                  , data=anes2008, family = binomial("logit"))
m8_2008[[2]] <- glm(vote_dem ~ harm_all + fair_all + ingr_all + auth_all
                    + pid_dem + pid_rep + relig + educ + age + female + black
                  , data=anes2008, family = binomial("logit"))

## simulation of predicted probabilities / first differences
m8_2008_res <- rbind(sim(m8_2008, iv=data.frame(harm_all = c(0,1)))
                   , sim(m8_2008, iv=data.frame(fair_all = c(0,1)))
                   , sim(m8_2008, iv=data.frame(ingr_all = c(0,1)))
                   , sim(m8_2008, iv=data.frame(auth_all = c(0,1))))
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


### Table for D2: social/economic ideology -> mft (logit)

stargazer(m2soceco[[1]], m2soceco[[5]], m2soceco[[2]], m2soceco[[6]]
        , m2soceco[[3]], m2soceco[[7]], m2soceco[[4]], m2soceco[[8]]
        , type="text", out="tab/m2soceco.tex"
        , title="Logit models predicting references to four moral foundations using two-dimensional conceptualization of ideology"
        , covariate.labels = c("Economic Liberalism","Social Liberalism",covLabs)
        , column.labels = rep(c("2012","2008"),4), model.numbers = FALSE
        , dep.var.labels=rev(gsub("\n","",mftLabs))
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2soceco", no.space=T, table.placement="ht"
)


### Table for D3 [fig 2, no leader]: ideology -> mft (logit)

stargazer(m2lead[[1]], m2lead[[5]], m2lead[[2]], m2lead[[6]]
        , m2lead[[3]], m2lead[[7]], m2lead[[4]], m2lead[[8]]
        , type="text", out="tab/m2lead.tex"
        , title="Logit models predicting references to four moral foundations using ideology (omitting leader from moral dictionary)"
        , covariate.labels = c("Conservative", "Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2lead", no.space=T, table.placement="ht"
          )


### Table for D4: ideology -> mft (virtues/vices)

## virtues
stargazer(m2val[[1]], m2val[[9]], m2val[[3]], m2val[[11]], m2val[[5]], m2val[[13]], m2val[[7]], m2val[[15]]
        , type="text", out="tab/m2virtue.tex"
        , title="Logit models predicting references to four moral foundations described as virtues using ideology"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2virtue", no.space=T, table.placement="ht"
          )

## vices
stargazer(m2val[[2]], m2val[[10]], m2val[[4]], m2val[[12]], m2val[[6]], m2val[[14]], m2val[[8]], m2val[[16]]
        , type="text", out="tab/m2vice.tex"
        , title="Logit models predicting references to four moral foundations described as vices using ideology"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2vice", no.space=T, table.placement="ht"
          )


### Table for D5: ideology -> mft (in-/out-party)

## in-party
stargazer(m2inout[[1]], m2inout[[9]], m2inout[[3]], m2inout[[11]]
        , m2inout[[5]], m2inout[[13]], m2inout[[7]], m2inout[[15]]
        , type="text", out="tab/m2inparty.tex"
        , title="Logit models predicting references to four moral foundations describing in-party using ideology"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2inparty", no.space=T, table.placement="ht"
          )

## out-party
stargazer(m2inout[[2]], m2inout[[10]], m2inout[[4]], m2inout[[12]]
        , m2inout[[6]], m2inout[[14]], m2inout[[8]], m2inout[[16]]
        , type="text", out="tab/m2outparty.tex"
        , title="Logit models predicting references to four moral foundations describing out-party using ideology"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate", covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2outparty", no.space=T, table.placement="ht"
)


### Table for D6: ideology -> mft (likes/dislikes)

## likes
stargazer(m2lidi[[1]], m2lidi[[9]], m2lidi[[3]], m2lidi[[11]]
        , m2lidi[[5]], m2lidi[[13]], m2lidi[[7]], m2lidi[[15]]
        , type="text", out="tab/m2likes.tex"
        , title="Logit models predicting references to four moral foundations using ideology (likes)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate",covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2likes", no.space=T, table.placement="ht"
          )

## dislikes
stargazer(m2lidi[[2]], m2lidi[[10]], m2lidi[[4]], m2lidi[[12]], m2lidi[[6]]
        , m2lidi[[14]], m2lidi[[8]], m2lidi[[16]]
        , type="text", out="tab/m2dislikes.tex"
        , title="Logit models predicting references to four moral foundations using ideology (dislikes)"
        , dep.var.labels = rev(gsub("\n","",mftLabs))
        , covariate.labels = c("Conservative","Moderate",covLabs)
        , column.labels = rep(c("2012","2008"),4)
        , model.numbers = FALSE, order=c(2,1,3:8)
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m2dislikes", no.space=T, table.placement="ht"
          )



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


### Table for D10 [fig 6 + 2008]: mft -> protest behavior index (ols)

stargazer(m6,m6_2008
        , type="text", out="tab/m6part.tex"
        , title="OLS models predicting protest behavior based on moral foundations"
        , covariate.labels=c(rev(gsub("\n","",mftLabs))
                             , "Strength of Party Identification", covLabs)
        , column.labels = c("2012","2008"), column.separate = c(2,2)
        , model.numbers = TRUE, keep.stat = c("n","rsq")
        , dep.var.labels="Vote for Democratic Presidential Candidate"
        , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
        , label="tab:m6part", no.space=T, table.placement="ht"
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


