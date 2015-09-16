##########################################################################################
# Project:  Moral foundations of Political Reasoning
# File:     mft_analyses.R
# Overview: this file contains the main analyses and generates all plots and tables
#           for the paper. Uses the datasets generated in mft_prep
# Author:   Patrick Kraft
# Date:     12/08/2014
##########################################################################################


rm(list=ls())
setwd("/data/Uni/projects/2014/mft/calc")

# load packages
pkg <- c("reshape2","ggplot2","stargazer","xtable")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(list=ls())

# I had to install an old Zelig version because the new one still had bugs...
# install.packages("func/Zelig_3.5.5.tar.gz",repos=NULL)
library(Zelig)

# load additional functions
source("func/anes_plot.R")

# load recoded dataset
load("out/anes.RData")
#load("out/anes_full.RData")


#####################################
# Data Overview: Dependent Variable #
#####################################

# plot overview
prop_plot(data=list(anes2008,anes2012), mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology: All Evaluations"
          , file = "fig/p1_mft_ideol.pdf")

prop_plot(data=list(anes2008,anes2012), mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology: Candidate Evaluations"
          , file = "fig/p2_mft_ideol_ca.pdf")

prop_plot(data=list(anes2008,anes2012), mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology: Party Evaluations"
          , file = "fig/p3_mft_ideol_pa.pdf")

prop_plot(data=list(anes2008,anes2012), mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
          , groupvarname="pid", legendname = "Party Identification", title = "Moral Foundation and Party Identification: All Evaluations"
          , file = "fig/a1_mft_pid.pdf")

prop_plot(data=list(anes2008,anes2012), mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
          , groupvarname="pid", legendname = "Party Identification", title = "Moral Foundation and Party Identification: Candidate Evaluations"
          , file = "fig/a2_mft_pid_ca.pdf")

prop_plot(data=list(anes2008,anes2012), mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
          , groupvarname="pid", legendname = "Party Identification", title = "Moral Foundation and Party Identification: Party Evaluations"
          , file = "fig/a3_mft_pid_pa.pdf")


###############################
# Data Overview: Missing Data #
###############################


### table for missing cases

tab_mis <- rbind(c(table(anes2008$spanish)[2],table(anes2008$spanish)[2]*100/sum(table(anes2008$spanish)))
                 , c(table(anes2012$spanish)[2],table(anes2012$spanish)[2]*100/sum(table(anes2012$spanish)))
                 , c(table(anes2008$num_total==0)[2],table(anes2008$num_total==0)[2]*100/sum(table(anes2008$num_total==0)))
                 , c(table(anes2012$num_total==0)[2],table(anes2012$num_total==0)[2]*100/sum(table(anes2012$num_total==0)))
                 , c(table(anes2008$num_ca==0)[2],table(anes2008$num_ca==0)[2]*100/sum(table(anes2008$num_ca==0)))
                 , c(table(anes2012$num_ca==0)[2],table(anes2012$num_ca==0)[2]*100/sum(table(anes2012$num_ca==0)))
                 , c(table(anes2008$num_pa==0)[2],table(anes2008$num_pa==0)[2]*100/sum(table(anes2008$num_pa==0)))
                 , c(table(anes2012$num_pa==0)[2],table(anes2012$num_pa==0)[2]*100/sum(table(anes2012$num_pa==0))))
colnames(tab_mis) <- c("N","Percent")
rownames(tab_mis) <- c("Spanish Interview (2008)", "Spanish Interview (2012)", "No Responses (Overall, 2008)", "No Responses (Overall, 2012)"
                       , "No Responses (Candidate Evaluations, 2008)", "No Responses (Candidate Evaluations, 2012)"
                       , "No Responses (Party Evaluations, 2008)", "No Responses (Party Evaluations, 2012)")
print(xtable(tab_mis, align="lcc",digits=c(0,0,2), caption = "Overview - Missing Open-Ended Responses"
             ,label="tab:a1_mis"),file="tab/a1_mis.tex")


### plot number of words
# not that some maximum values are omitted!
a0a <- qplot(num_total, data=anes2008[anes2008$num_total>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2008)")
a0b <- qplot(num_total, data=anes2012[anes2012$num_total>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("All Open-Ended Responses (2012)")
a0c <- qplot(num_ca, data=anes2008[anes2008$num_ca>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2008)")
a0d <- qplot(num_ca, data=anes2012[anes2012$num_ca>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Candidate Evaluations (2012)")
a0e <- qplot(num_pa, data=anes2008[anes2008$num_pa>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2008)")
a0f <- qplot(num_pa, data=anes2012[anes2012$num_pa>0, ], geom="bar", binwidth = 1
             , ylab = "Frequency", xlab = "Number of Words") +
  theme_bw() + scale_x_continuous(limits=c(1,200)) + ggtitle("Party Evaluations (2012)")
pdf("fig/a0_num.pdf")
multiplot(a0a, a0c, a0e, a0b, a0d, a0f, cols=2)
dev.off()


############
# Analyses #
############
# question: control for log(num_total) would be preferred, but some models give an error then...


### models predicting references to moral foundations based on ideology

# model estimation
m1_2008_harm <- zelig(harm_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2012_harm <- zelig(harm_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1_2008_fair <- zelig(fair_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2012_fair <- zelig(fair_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1_2008_ingr <- zelig(ingr_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2012_ingr <- zelig(ingr_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1_2008_auth <- zelig(auth_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2012_auth <- zelig(auth_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m1_2008_harm, m1_2012_harm, m1_2008_fair, m1_2012_fair, m1_2008_ingr, m1_2012_ingr, m1_2008_auth, m1_2012_auth
          , type="text", out="tab/m1_mft.tex"
          , title="Logit Models Predicting References to four Moral Foundations using Ideology"
          , covariate.labels = c("Conservative","Moderate","Church Attendance","Education (College Degree)"
                                 ,"Age","Sex (Female)","Race (African American)","Number of Words")
          , column.labels = c("2008","2012","2008","2012","2008","2012","2008","2012")
          , model.numbers = FALSE, order=c(2,1,3:8)
          , dep.var.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect")
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m1_mft", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m1_res <- data.frame(NULL)
mlist <- list(m1_2008_harm, m1_2012_harm, m1_2008_fair, m1_2012_fair, m1_2008_ingr, m1_2012_ingr, m1_2008_auth, m1_2012_auth)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], ideol = c("Liberal", "Conservative"))
  sim <- sim(mlist[[i]], x=x)
  m1_res <- rbind(m1_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
colnames(m1_res) <- c("mean","cilo","cihi")
m1_res$var <- rep((length(mlist)/2):1,each=2)
m1_res$year <- rep(c("2008","2012"),4)
ggplot(m1_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority / \nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m1_mft.pdf")

# blank plot
ggplot(m1_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
geom_blank() + labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority / \nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m1_mft_blank.pdf")


### models predicting references to moral foundations based on social/economic dimension

# combine issue positions to dimension (mean)
# high values -> more liberal position
anes2008$ideol_econ <- with(anes2008, (issue_aid + issue_govspend - issue_medins + 1)/3)
anes2008$ideol_social <- anes2008$issue_gay
anes2012$ideol_econ <- with(anes2012, (issue_aid + issue_govspend - issue_medins - issue_jobs + 2)/4)
anes2012$ideol_social <- with(anes2012, (issue_gay + issue_abort)/2)


# check factor analysis for 2012
factanal(na.omit(anes2012[,grep("issue",names(anes2012))]), 1, rotation="varimax")
factanal(na.omit(anes2012[,grep("issue",names(anes2012))]), 2, rotation="varimax")

# model estimation
m1b_2008_harm <- zelig(harm_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1b_2012_harm <- zelig(harm_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1b_2008_fair <- zelig(fair_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1b_2012_fair <- zelig(fair_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1b_2008_ingr <- zelig(ingr_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1b_2012_ingr <- zelig(ingr_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1b_2008_auth <- zelig(auth_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1b_2012_auth <- zelig(auth_all ~ ideol_econ + ideol_social + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m1b_2008_harm, m1b_2012_harm, m1b_2008_fair, m1b_2012_fair, m1b_2008_ingr, m1b_2012_ingr, m1b_2008_auth, m1b_2012_auth
          , type="text", out="tab/m1b_mft.tex"
          , title="Logit Models Predicting References to four Moral Foundations using Two-dimensional Conceptualization of Ideology"
          , covariate.labels = c("Economic Liberalism","Social Liberalism","Moderate","Church Attendance","Education (College Degree)"
                                 ,"Age","Sex (Female)","Race (African American)","Number of Words")
          , column.labels = c("2008","2012","2008","2012","2008","2012","2008","2012")
          , model.numbers = FALSE
          , dep.var.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect")
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m1b_mft", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m1b_res <- data.frame(NULL)
mlist <- list(m1b_2008_harm, m1b_2012_harm, m1b_2008_fair, m1b_2012_fair
            , m1b_2008_ingr, m1b_2012_ingr, m1b_2008_auth, m1b_2012_auth)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], ideol_econ = c(1,0))
  sim <- sim(mlist[[i]], x=x)
  m1b_res <- rbind(m1b_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                            , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
  x <- setx(mlist[[i]], ideol_social = c(1,0))
  sim <- sim(mlist[[i]], x=x)
  m1b_res <- rbind(m1b_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                            , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
colnames(m1b_res) <- c("mean","cilo","cihi")
m1b_res$Ideology <- rep(c("Economic Dimension","Social Dimension"),8)
m1b_res$var <- rep((length(mlist)/2):1,each=4)
m1b_res$year <- rep(c("2008","2008","2012","2012"),4)
ggplot(m1b_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority / \nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare")) +
  facet_grid(Ideology ~ .)
ggsave(filename = "fig/m1b_mft.pdf")

  
### models predicting vote choice based on moral considerations

# model estimation
m2_2008_vote1 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2_2012_vote1 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)
m2_2008_vote2 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2_2012_vote2 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2_2008_vote1,m2_2008_vote2,m2_2012_vote1,m2_2012_vote2
          , type="text", out="tab/m2_vote.tex"
          , title="Logit Models Predicting Democratic Vote Choice Based on Moral Foundations"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Party Identification (Democrat)", "Party Identification (Republican)"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2_vote", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m2_res <- data.frame()
mlist <- list(m2_2008_vote1, m2_2012_vote1, m2_2008_vote2, m2_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2_res <- rbind(m2_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2_res) <- c("mean","cilo","cihi")
m2_res$var <- rep(4:1,4)
m2_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2_res$cond <- rep(c("No", "Yes"), each=8)
ggplot(m2_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Vote for Democratic Candidate") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m2_vote.pdf")


### models predicting general references to moral foundations depending on political knowledge, media exposure, political discussions

# estimate models
m3_2008_polknow   <- zelig(mft_all ~ polknow + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2012_polknow   <- zelig(mft_all ~ polknow + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3_2008_polmedia  <- zelig(mft_all ~ polmedia + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2012_polmedia  <- zelig(mft_all ~ polmedia + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3_2008_poldisc   <- zelig(mft_all ~ poldisc + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2012_poldisc   <- zelig(mft_all ~ poldisc + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3_2008_all       <- zelig(mft_all ~ polknow + polmedia + poldisc + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2012_all       <- zelig(mft_all ~ polknow + polmedia + poldisc + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m3_2008_polknow, m3_2012_polknow, m3_2008_polmedia, m3_2012_polmedia, m3_2008_poldisc, m3_2012_poldisc, m3_2008_all, m3_2012_all
          , type="text", out="tab/m3_learn.tex"
          , title="Logit Models Predicting Overall References to Moral Foundations"
          , covariate.labels=c("Political Knowledge","Political Media Exposure","Political Discussions"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)"
                               ,"Number of Words")
          , column.labels = rep(c("2008","2012"),4), model.numbers = TRUE
          , dep.var.labels="Reference to any Moral Foundation"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m3_learn", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m3_res <- data.frame()
mlist <- list(m3_2008_polknow, m3_2008_polmedia, m3_2008_poldisc)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], polknow=c(max(anes2008$polknow, na.rm=T),min(anes2008$polknow, na.rm=T))
            , polmedia=c(max(anes2008$polmedia, na.rm=T),min(anes2008$polmedia, na.rm=T))
            , poldisc=c(max(anes2008$poldisc, na.rm=T),min(anes2008$poldisc, na.rm=T))
            )
  sim <- sim(mlist[[i]], x=x)
  m3_res <- rbind(m3_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975))))
}
mlist <- list(m3_2012_polknow, m3_2012_polmedia, m3_2012_poldisc)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], polknow=c(max(anes2012$polknow, na.rm=T),min(anes2012$polknow, na.rm=T))
            , polmedia=c(max(anes2012$polmedia, na.rm=T),min(anes2012$polmedia, na.rm=T))
            , poldisc=c(max(anes2012$poldisc, na.rm=T),min(anes2012$poldisc, na.rm=T))
  )
  sim <- sim(mlist[[i]], x=x)
  m3_res <- rbind(m3_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975))))
}
for(j in 1:3){
  x <- setx(m3_2008_all, polknow=c(max(anes2008$polknow, na.rm=T),min(anes2008$polknow, na.rm=T))*(j==1)
            , polmedia=c(max(anes2008$polmedia, na.rm=T),min(anes2008$polmedia, na.rm=T))*(j==2)
            , poldisc=c(max(anes2008$poldisc, na.rm=T),min(anes2008$poldisc, na.rm=T))*(j==3))
  sim <- sim(m3_2008_all, x=x)
  m3_res <- rbind(m3_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
for(j in 1:3){
  x <- setx(m3_2012_all, polknow=c(max(anes2012$polknow, na.rm=T),min(anes2012$polknow, na.rm=T))*(j==1)
            , polmedia=c(max(anes2012$polmedia, na.rm=T),min(anes2012$polmedia, na.rm=T))*(j==2)
            , poldisc=c(max(anes2012$poldisc, na.rm=T),min(anes2012$poldisc, na.rm=T))*(j==3))
  sim <- sim(m3_2012_all, x=x)
  m3_res <- rbind(m3_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
  )
}
colnames(m3_res) <- c("mean","cilo","cihi")
m3_res$var <- rep(3:1,4)
m3_res$year <- rep(c("2008","2012","2008","2012"),each = 3)
m3_res$cond <- rep(c("No", "Yes"), each=6)
ggplot(m3_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable", x= "Change in Probability") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference any Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for Both Remaining Variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=3:1, labels=c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions"))
ggsave(filename = "fig/m3_learn.pdf")

