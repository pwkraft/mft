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

# plot overview: no leader in auth mft
prop_plot(data=list(anes2008noleader,anes2012noleader), mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology: All Evaluations (no leader)"
        , file = "fig/p1_mft_ideol_noleader.pdf")

prop_plot(data=list(anes2008noleader,anes2012noleader), mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology: Candidate Evaluations (no leader)"
          , file = "fig/p2_mft_ideol_ca_noleader.pdf")

prop_plot(data=list(anes2008noleader,anes2012noleader), mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
          , groupvarname="ideol", legendname = "Ideology", title = "Moral Foundation and Ideology: Party Evaluations (no leader)"
          , file = "fig/p3_mft_ideol_pa_noleader.pdf")

prop_plot(data=list(anes2008noleader,anes2012noleader), mftvarnames=c("puri_all", "auth_all", "ingr_all", "fair_all", "harm_all")
          , groupvarname="pid", legendname = "Party Identification", title = "Moral Foundation and Party Identification: All Evaluations (no leader)"
          , file = "fig/a1_mft_pid_noleader.pdf")

prop_plot(data=list(anes2008noleader,anes2012noleader), mftvarnames=c("puri_ca", "auth_ca", "ingr_ca", "fair_ca", "harm_ca")
          , groupvarname="pid", legendname = "Party Identification", title = "Moral Foundation and Party Identification: Candidate Evaluations (no leader)"
          , file = "fig/a2_mft_pid_ca_noleader.pdf")

prop_plot(data=list(anes2008noleader,anes2012noleader), mftvarnames=c("puri_pa", "auth_pa", "ingr_pa", "fair_pa", "harm_pa")
          , groupvarname="pid", legendname = "Party Identification", title = "Moral Foundation and Party Identification: Party Evaluations (no leader)"
          , file = "fig/a3_mft_pid_pa_noleader.pdf")


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


### models predicting references to moral foundations based on ideology (noleader)

# model estimation
m1_2008_harm_noleader <- zelig(harm_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008noleader, model="logit",cite=F)
m1_2012_harm_noleader <- zelig(harm_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012noleader, model="logit",cite=F)
m1_2008_fair_noleader <- zelig(fair_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008noleader, model="logit",cite=F)
m1_2012_fair_noleader <- zelig(fair_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012noleader, model="logit",cite=F)
m1_2008_ingr_noleader <- zelig(ingr_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008noleader, model="logit",cite=F)
m1_2012_ingr_noleader <- zelig(ingr_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012noleader, model="logit",cite=F)
m1_2008_auth_noleader <- zelig(auth_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008noleader, model="logit",cite=F)
m1_2012_auth_noleader <- zelig(auth_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012noleader, model="logit",cite=F)

# generate table
stargazer(m1_2008_harm_noleader, m1_2012_harm_noleader, m1_2008_fair_noleader, m1_2012_fair_noleader, m1_2008_ingr_noleader, m1_2012_ingr_noleader, m1_2008_auth_noleader, m1_2012_auth_noleader
          , type="text", out="tab/m1_mft_noleader.tex"
          , title="Logit Models Predicting References to four Moral Foundations using Ideology (no leader)"
          , covariate.labels = c("Conservative","Moderate","Church Attendance","Education (College Degree)"
                                 ,"Age","Sex (Female)","Race (African American)","Number of Words")
          , column.labels = c("2008","2012","2008","2012","2008","2012","2008","2012")
          , model.numbers = FALSE, order=c(2,1,3:8)
          , dep.var.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect")
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m1_mft", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m1_res_noleader <- data.frame(NULL)
mlist <- list(m1_2008_harm_noleader, m1_2012_harm_noleader, m1_2008_fair_noleader, m1_2012_fair_noleader, m1_2008_ingr_noleader, m1_2012_ingr_noleader, m1_2008_auth_noleader, m1_2012_auth_noleader)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], ideol = c("Liberal", "Conservative"))
  sim <- sim(mlist[[i]], x=x)
  m1_res_noleader <- rbind(m1_res_noleader,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
colnames(m1_res_noleader) <- c("mean","cilo","cihi")
m1_res_noleader$var <- rep((length(mlist)/2):1,each=2)
m1_res_noleader$year <- rep(c("2008","2012"),4)
ggplot(m1_res_noleader, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(no leader)") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority / \nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m1_mft_noleader.pdf")


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


### models predicting references to moral foundations based on virtues/vices

# model estimation
m1c_2008_harm_virtue <- zelig(harm_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2008_harm_vice <- zelig(harm_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2012_harm_virtue <- zelig(harm_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1c_2012_harm_vice <- zelig(harm_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1c_2008_fair_virtue <- zelig(fair_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2008_fair_vice <- zelig(fair_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2012_fair_virtue <- zelig(fair_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1c_2012_fair_vice <- zelig(fair_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1c_2008_ingr_virtue <- zelig(ingr_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2008_ingr_vice <- zelig(ingr_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2012_ingr_virtue <- zelig(ingr_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1c_2012_ingr_vice <- zelig(ingr_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1c_2008_auth_virtue <- zelig(auth_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2008_auth_vice <- zelig(auth_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1c_2012_auth_virtue <- zelig(auth_virtue_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1c_2012_auth_vice <- zelig(auth_vice_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m1c_2008_harm_virtue, m1c_2012_harm_virtue, m1c_2008_harm_vice, m1c_2012_harm_vice, m1c_2008_fair_virtue, m1c_2012_fair_virtue, m1c_2008_fair_vice, m1c_2012_fair_vice, m1c_2008_ingr_virtue, m1c_2012_ingr_virtue, m1c_2008_ingr_vice, m1c_2012_ingr_vice, m1c_2008_auth_virtue, m1c_2012_auth_virtue, m1c_2008_auth_vice, m1c_2012_auth_vice
          , type="text", out="tab/m1c_mft.tex"
          , title="Logit Models Predicting References to four Moral Foundations using Ideology (by valence)"
          , covariate.labels = c("Conservative","Moderate","Church Attendance","Education (College Degree)"
                                 ,"Age","Sex (Female)","Race (African American)","Number of Words")
          , column.labels = c("2008","2012","2008","2012","2008","2012","2008","2012")
          , model.numbers = FALSE, order=c(2,1,3:8)
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m1_mft", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m1c_res <- data.frame(NULL)
mlist <- list(m1c_2008_harm_virtue, m1c_2012_harm_virtue, m1c_2008_harm_vice, m1c_2012_harm_vice, m1c_2008_fair_virtue, m1c_2012_fair_virtue, m1c_2008_fair_vice, m1c_2012_fair_vice, m1c_2008_ingr_virtue, m1c_2012_ingr_virtue, m1c_2008_ingr_vice, m1c_2012_ingr_vice, m1c_2008_auth_virtue, m1c_2012_auth_virtue, m1c_2008_auth_vice, m1c_2012_auth_vice)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], ideol = c("Liberal", "Conservative"))
  sim <- sim(mlist[[i]], x=x)
  m1c_res <- rbind(m1c_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
colnames(m1c_res) <- c("mean","cilo","cihi")
m1c_res$var <- rep((length(mlist)/2):1,each=2)
m1c_res$year <- rep(c("2008","2012"),8)
ggplot(m1c_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(by valence)") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:8, labels=c("Authority\n(vice)","Authority\n(virtue)", "Ingroup\n(vice)","Ingroup\n(virtue)", "Fairness\n(vice)", "Fairness\n(virtue)", "Harm\n(vice)", "Harm\n(virtue)"))
ggsave(filename = "fig/m1c_mft.pdf")


### models predicting references to moral foundations for in/out-party

# in-party DV
anes2008$harm_in <- anes2008$harm_dem
anes2008$harm_in[which(anes2008$vote_dem==0)] <- anes2008$harm_rep[which(anes2008$vote_dem==0)]
anes2008$fair_in <- anes2008$fair_dem
anes2008$fair_in[which(anes2008$vote_dem==0)] <- anes2008$fair_rep[which(anes2008$vote_dem==0)]
anes2008$ingr_in <- anes2008$ingr_dem
anes2008$ingr_in[which(anes2008$vote_dem==0)] <- anes2008$ingr_rep[which(anes2008$vote_dem==0)]
anes2008$auth_in <- anes2008$auth_dem
anes2008$auth_in[which(anes2008$vote_dem==0)] <- anes2008$auth_rep[which(anes2008$vote_dem==0)]
anes2012$harm_in <- anes2012$harm_dem
anes2012$harm_in[which(anes2012$vote_dem==0)] <- anes2012$harm_rep[which(anes2012$vote_dem==0)]
anes2012$fair_in <- anes2012$fair_dem
anes2012$fair_in[which(anes2012$vote_dem==0)] <- anes2012$fair_rep[which(anes2012$vote_dem==0)]
anes2012$ingr_in <- anes2012$ingr_dem
anes2012$ingr_in[which(anes2012$vote_dem==0)] <- anes2012$ingr_rep[which(anes2012$vote_dem==0)]
anes2012$auth_in <- anes2012$auth_dem
anes2012$auth_in[which(anes2012$vote_dem==0)] <- anes2012$auth_rep[which(anes2012$vote_dem==0)]

# out-party DV
anes2008$harm_out <- anes2008$harm_dem
anes2008$harm_out[which(anes2008$vote_dem==1)] <- anes2008$harm_rep[which(anes2008$vote_dem==1)]
anes2008$fair_out <- anes2008$fair_dem
anes2008$fair_out[which(anes2008$vote_dem==1)] <- anes2008$fair_rep[which(anes2008$vote_dem==1)]
anes2008$ingr_out <- anes2008$ingr_dem
anes2008$ingr_out[which(anes2008$vote_dem==1)] <- anes2008$ingr_rep[which(anes2008$vote_dem==1)]
anes2008$auth_out <- anes2008$auth_dem
anes2008$auth_out[which(anes2008$vote_dem==1)] <- anes2008$auth_rep[which(anes2008$vote_dem==1)]
anes2012$harm_out <- anes2012$harm_dem
anes2012$harm_out[which(anes2012$vote_dem==1)] <- anes2012$harm_rep[which(anes2012$vote_dem==1)]
anes2012$fair_out <- anes2012$fair_dem
anes2012$fair_out[which(anes2012$vote_dem==1)] <- anes2012$fair_rep[which(anes2012$vote_dem==1)]
anes2012$ingr_out <- anes2012$ingr_dem
anes2012$ingr_out[which(anes2012$vote_dem==1)] <- anes2012$ingr_rep[which(anes2012$vote_dem==1)]
anes2012$auth_out <- anes2012$auth_dem
anes2012$auth_out[which(anes2012$vote_dem==1)] <- anes2012$auth_rep[which(anes2012$vote_dem==1)]

# model estimation
m1d_2008_harm_in <- zelig(harm_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2008_harm_out <- zelig(harm_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2012_harm_in <- zelig(harm_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1d_2012_harm_out <- zelig(harm_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1d_2008_fair_in <- zelig(fair_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2008_fair_out <- zelig(fair_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2012_fair_in <- zelig(fair_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1d_2012_fair_out <- zelig(fair_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1d_2008_ingr_in <- zelig(ingr_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2008_ingr_out <- zelig(ingr_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2012_ingr_in <- zelig(ingr_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1d_2012_ingr_out <- zelig(ingr_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1d_2008_auth_in <- zelig(auth_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2008_auth_out <- zelig(auth_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1d_2012_auth_in <- zelig(auth_in ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1d_2012_auth_out <- zelig(auth_out ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m1d_2008_harm_in, m1d_2012_harm_in, m1d_2008_harm_out, m1d_2012_harm_out, m1d_2008_fair_in, m1d_2012_fair_in, m1d_2008_fair_out, m1d_2012_fair_out, m1d_2008_ingr_in, m1d_2012_ingr_in, m1d_2008_ingr_out, m1d_2012_ingr_out, m1d_2008_auth_in, m1d_2012_auth_in, m1d_2008_auth_out, m1d_2012_auth_out
          , type="text", out="tab/m1d_mft.tex"
          , title="Logit Models Predicting References to four Moral Foundations using Ideology (by in-party/out-party)"
          , covariate.labels = c("Conservative","Moderate","Church Attendance","Education (College Degree)"
                                 ,"Age","Sex (Female)","Race (African American)","Number of Words")
          , column.labels = c("2008","2012","2008","2012","2008","2012","2008","2012")
          , model.numbers = FALSE, order=c(2,1,3:8)
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m1_mft", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m1d_res <- data.frame(NULL)
mlist <- list(m1d_2008_harm_in, m1d_2012_harm_in, m1d_2008_harm_out, m1d_2012_harm_out, m1d_2008_fair_in, m1d_2012_fair_in, m1d_2008_fair_out, m1d_2012_fair_out, m1d_2008_ingr_in, m1d_2012_ingr_in, m1d_2008_ingr_out, m1d_2012_ingr_out, m1d_2008_auth_in, m1d_2012_auth_in, m1d_2008_auth_out, m1d_2012_auth_out)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], ideol = c("Liberal", "Conservative"))
  sim <- sim(mlist[[i]], x=x)
  m1d_res <- rbind(m1d_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
colnames(m1d_res) <- c("mean","cilo","cihi")
m1d_res$var <- rep((length(mlist)/2):1,each=2)
m1d_res$year <- rep(c("2008","2012"),8)
ggplot(m1d_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(by in-party/out-party)") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:8, labels=c("Authority\n(out)","Authority\n(in)", "Ingroup\n(out)","Ingroup\n(in)", "Fairness\n(out)", "Fairness\n(in)", "Harm\n(out)", "Harm\n(in)"))
ggsave(filename = "fig/m1d_mft.pdf")


### models predicting references to moral foundations for likes/dislikes

# model estimation
m1e_2008_harm_li <- zelig(harm_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2008_harm_di <- zelig(harm_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2012_harm_li <- zelig(harm_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1e_2012_harm_di <- zelig(harm_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1e_2008_fair_li <- zelig(fair_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2008_fair_di <- zelig(fair_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2012_fair_li <- zelig(fair_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1e_2012_fair_di <- zelig(fair_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1e_2008_ingr_li <- zelig(ingr_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2008_ingr_di <- zelig(ingr_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2012_ingr_li <- zelig(ingr_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1e_2012_ingr_di <- zelig(ingr_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m1e_2008_auth_li <- zelig(auth_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2008_auth_di <- zelig(auth_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1e_2012_auth_li <- zelig(auth_li ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1e_2012_auth_di <- zelig(auth_di ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m1e_2008_harm_li, m1e_2012_harm_li, m1e_2008_harm_di, m1e_2012_harm_di, m1e_2008_fair_li, m1e_2012_fair_li, m1e_2008_fair_di, m1e_2012_fair_di, m1e_2008_ingr_li, m1e_2012_ingr_li, m1e_2008_ingr_di, m1e_2012_ingr_di, m1e_2008_auth_li, m1e_2012_auth_li, m1e_2008_auth_di, m1e_2012_auth_di
          , type="text", out="tab/m1e_mft.tex"
          , title="Logit Models Predicting References to four Moral Foundations using Ideology (by like/dislike)"
          , covariate.labels = c("Conservative","Moderate","Church Attendance","Education (College Degree)"
                                 ,"Age","Sex (Female)","Race (African American)","Number of Words")
          , column.labels = c("2008","2012","2008","2012","2008","2012","2008","2012")
          , model.numbers = FALSE, order=c(2,1,3:8)
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m1_mft", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m1e_res <- data.frame(NULL)
mlist <- list(m1e_2008_harm_li, m1e_2012_harm_li, m1e_2008_harm_di, m1e_2012_harm_di, m1e_2008_fair_li, m1e_2012_fair_li, m1e_2008_fair_di, m1e_2012_fair_di, m1e_2008_ingr_li, m1e_2012_ingr_li, m1e_2008_ingr_di, m1e_2012_ingr_di, m1e_2008_auth_li, m1e_2012_auth_li, m1e_2008_auth_di, m1e_2012_auth_di)
for(i in 1:length(mlist)){
  x <- setx(mlist[[i]], ideol = c("Liberal", "Conservative"))
  sim <- sim(mlist[[i]], x=x)
  m1e_res <- rbind(m1e_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                           , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
                  )
}
colnames(m1e_res) <- c("mean","cilo","cihi")
m1e_res$var <- rep((length(mlist)/2):1,each=2)
m1e_res$year <- rep(c("2008","2012"),8)
ggplot(m1e_res, aes(x = mean, y = var-.052+.11*(year=="2008"), shape=year, color = year)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Dependent Variable: Moral Foundation"
       , x= "Conservatives more likey                                                       Liberals more likely") + 
  geom_vline(xintercept=0) + theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Reference each Moral Foundation\n(by like/dislike)") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year")) +
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=1:8, labels=c("Authority\n(di)","Authority\n(li)", "Ingroup\n(di)","Ingroup\n(li)", "Fairness\n(di)", "Fairness\n(li)", "Harm\n(di)", "Harm\n(li)"))
ggsave(filename = "fig/m1e_mft.pdf")


### models predicting references to moral foundations for in/out-party including virtues/vices
## skipped for now


### models predicting references to moral foundations for likes/disliks including virtues/vices
## skipped for now

  
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


### models predicting turnout based on moral foundations

# model estimation
m2b_2008_vote1 <- zelig(vote ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2b_2012_vote1 <- zelig(vote ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m2b_2008_vote2 <- zelig(vote ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2b_2012_vote2 <- zelig(vote ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2b_2008_vote1,m2b_2008_vote2,m2b_2012_vote1,m2b_2012_vote2
          , type="text", out="tab/m2b_vote.tex"
          , title="Logit Models Predicting Turnout Based on Moral Foundations"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Strength of Party Identification"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Turnout"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2b_vote", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m2b_res <- data.frame()
mlist <- list(m2b_2008_vote1, m2b_2012_vote1, m2b_2008_vote2, m2b_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2b_res <- rbind(m2b_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2b_res) <- c("mean","cilo","cihi")
m2b_res$var <- rep(4:1,4)
m2b_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2b_res$cond <- rep(c("No", "Yes"), each=8)
ggplot(m2b_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Participate in Election") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m2b_vote.pdf")


### mft X ideology -> turnout

# model estimation
m2b_2008_vote1 <- zelig(vote ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2b_2012_vote1 <- zelig(vote ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m2b_2008_vote2 <- zelig(vote ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + pid_str*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2b_2012_vote2 <- zelig(vote ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2b_2008_vote1,m2b_2008_vote2,m2b_2012_vote1,m2b_2012_vote2
          , type="text", out="tab/m2b_vote2.tex"
          , title="Logit Models Predicting Turnout Based on Moral Foundations"
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Turnout"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2b_vote2", no.space=T, table.placement="ht"
)


### models predicting protest behavior based on moral foundations

# model estimation
m2c_2008_vote1 <- zelig(protest ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2c_2012_vote1 <- zelig(protest ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m2c_2008_vote2 <- zelig(protest ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2c_2012_vote2 <- zelig(protest ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2c_2008_vote1,m2c_2008_vote2,m2c_2012_vote1,m2c_2012_vote2
          , type="text", out="tab/m2c_vote.tex"
          , title="Logit Models Predicting Turnout Choice Based on Moral Foundations"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Strength of Party Identification"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2c_vote", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m2c_res <- data.frame()
mlist <- list(m2c_2008_vote1, m2c_2012_vote1, m2c_2008_vote2, m2c_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2c_res <- rbind(m2c_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2c_res) <- c("mean","cilo","cihi")
m2c_res$var <- rep(4:1,4)
m2c_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2c_res$cond <- rep(c("No", "Yes"), each=8)
ggplot(m2c_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Participate in Protest") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m2c_vote.pdf")


### models predicting petition based on moral foundations

# model estimation
m2d_2008_vote1 <- zelig(petition ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2d_2012_vote1 <- zelig(petition ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m2d_2008_vote2 <- zelig(petition ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2d_2012_vote2 <- zelig(petition ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2d_2008_vote1,m2d_2008_vote2,m2d_2012_vote1,m2d_2012_vote2
          , type="text", out="tab/m2d_vote.tex"
          , title="Logit Models Predicting Petition Signing Based on Moral Foundations"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Strength of Party Identification"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2d_vote", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m2d_res <- data.frame()
mlist <- list(m2d_2008_vote1, m2d_2012_vote1, m2d_2008_vote2, m2d_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2d_res <- rbind(m2d_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2d_res) <- c("mean","cilo","cihi")
m2d_res$var <- rep(4:1,4)
m2d_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2d_res$cond <- rep(c("No", "Yes"), each=8)
ggplot(m2d_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Sign a Petition") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m2d_vote.pdf")


### models predicting button based on moral foundations

# model estimation
m2e_2008_vote1 <- zelig(button ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2e_2012_vote1 <- zelig(button ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m2e_2008_vote2 <- zelig(button ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m2e_2012_vote2 <- zelig(button ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2e_2008_vote1,m2e_2008_vote2,m2e_2012_vote1,m2e_2012_vote2
          , type="text", out="tab/m2e_vote.tex"
          , title="Logit Models Predicting Wearing Button/Sign Based on Moral Foundations"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Strength of Party Identification"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2e_vote", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m2e_res <- data.frame()
mlist <- list(m2e_2008_vote1, m2e_2012_vote1, m2e_2008_vote2, m2e_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2e_res <- rbind(m2e_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2e_res) <- c("mean","cilo","cihi")
m2e_res$var <- rep(4:1,4)
m2e_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2e_res$cond <- rep(c("No", "Yes"), each=8)
ggplot(m2e_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Probability") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Predicted Probabilities to Show Button/Sign") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m2e_vote.pdf")


### models predicting combined index of non-voting participation based on moral foundations

# recode variables
anes2008$part <- with(anes2008, protest + petition + button)
anes2012$part <- with(anes2012, protest + petition + button)

# model estimation
m2e_2008_part1 <- zelig(part ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2008, model="ls",cite=F, robust=T)
m2e_2012_part1 <- zelig(part ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black + num_total, data=anes2012, model="ls",cite=F, robust=T)
m2e_2008_part2 <- zelig(part ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2008, model="ls",cite=F, robust=T)
m2e_2012_part2 <- zelig(part ~ harm_all + fair_all + ingr_all + auth_all + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="ls",cite=F, robust=T)

# generate table
stargazer(m2e_2008_part1,m2e_2008_part2,m2e_2012_part1,m2e_2012_part2
          , type="text", out="tab/m2e_part.tex"
          , title="OLS Models Predicting Protest Behavior Index Based on Moral Foundations"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Strength of Party Identification"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2e_part", no.space=T, table.placement="ht"
)

# Plot predicted probabilities / expected values
m2e_res <- data.frame()
mlist <- list(m2e_2008_part1, m2e_2012_part1, m2e_2008_part2, m2e_2012_part2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2e_res <- rbind(m2e_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2e_res) <- c("mean","cilo","cihi")
m2e_res$var <- rep(4:1,4)
m2e_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2e_res$cond <- rep(c("No", "Yes"), each=8)
ggplot(m2e_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Protest Index") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Protest Behavior Index") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for PID Strength")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                          , "Fairness / \nReciprocity", "Harm / \nCare"))
ggsave(filename = "fig/m2e_part.pdf")


### moral foundations X ideology -> protest behavior

# model estimation
m2e_2008_part1 <- zelig(part ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="ls",cite=F, robust=T)
m2e_2012_part1 <- zelig(part ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="ls",cite=F, robust=T)
m2e_2008_part2 <- zelig(part ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + pid_str + relig + educ + age + female + black + num_total, data=anes2008, model="ls",cite=F, robust=T)
m2e_2012_part2 <- zelig(part ~ harm_all*ideol + fair_all*ideol + ingr_all*ideol + auth_all*ideol + pid_str + relig + educ + age + female + black + num_total, data=anes2012, model="ls",cite=F, robust=T)

# generate table
stargazer(m2e_2008_part1,m2e_2008_part2,m2e_2012_part1,m2e_2012_part2
          , type="text", out="tab/m2e_part2.tex"
          , title="OLS Models Predicting Protest Behavior Index Based on Moral Foundations"
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2e_part2", no.space=T, table.placement="ht"
)


### models predicting party evaluations based on moral foundations

# model estimation
m2f_2008_vote1 <- zelig(eval_party ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2f_2012_vote1 <- zelig(eval_party ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)
m2f_2008_vote2 <- zelig(eval_party ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2f_2012_vote2 <- zelig(eval_party ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

# generate table
stargazer(m2f_2008_vote1,m2f_2008_vote2,m2f_2012_vote1,m2f_2012_vote2
          , type="text", out="tab/m2f_vote.tex"
          , title="Linear Model Predicting Feeling Thermometer Differential (Parties)"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Party Identification (Democrats)", "Party Identification (Republicans)"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2f_vote", no.space=T, table.placement="ht"
)

# Prepare plot predicted probabilities / expected values
m2f_res <- data.frame()
mlist <- list(m2f_2008_vote1, m2f_2012_vote1, m2f_2008_vote2, m2f_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2f_res <- rbind(m2f_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2f_res) <- c("mean","cilo","cihi")
m2f_res$var <- rep(4:1,4)
m2f_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2f_res$cond <- rep(c("No", "Yes"), each=8)
m2f_res$dv <- "Party Evaluation"


### models predicting candidate evaluations based on moral foundations

# model estimation
m2g_2008_vote1 <- zelig(eval_cand ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2g_2012_vote1 <- zelig(eval_cand ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)
m2g_2008_vote2 <- zelig(eval_cand ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2g_2012_vote2 <- zelig(eval_cand ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

# generate table
stargazer(m2g_2008_vote1,m2g_2008_vote2,m2g_2012_vote1,m2g_2012_vote2
          , type="text", out="tab/m2g_vote.tex"
          , title="Linear Model Predicting Feeling Thermometer Differential (Candidates)"
          , covariate.labels=c("Harm / Care","Fairness / Reciprocity","Ingroup / Loyalty","Authority / Respect"
                               , "Party Identification (Democrats)", "Party Identification (Republicans)"
                               ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          , column.labels = c("2008","2012"), column.separate = c(2,2), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Presidential Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m2g_vote", no.space=T, table.placement="ht"
)

# Prepare plot of predicted probabilities / expected values
m2g_res <- data.frame()
mlist <- list(m2g_2008_vote1, m2g_2012_vote1, m2g_2008_vote2, m2g_2012_vote2)
for(i in 1:length(mlist)){
  for(j in 1:4){
    x <- setx(mlist[[i]], harm_all = c(1,0)*(j==1), fair_all=c(1,0)*(j==2), ingr_all=c(1,0)*(j==3), auth_all=c(1,0)*(j==4))
    sim <- sim(mlist[[i]], x=x)
    m2g_res <- rbind(m2g_res,c(mean(sim$qi$ev[,1] - sim$qi$ev[,2])
                             , quantile(sim$qi$ev[,1] - sim$qi$ev[,2], probs=c(0.025,0.975)))
    )
  }
}
colnames(m2g_res) <- c("mean","cilo","cihi")
m2g_res$var <- rep(4:1,4)
m2g_res$year <- rep(c("2008","2012","2008","2012"),each = 4)
m2g_res$cond <- rep(c("No", "Yes"), each=8)
m2g_res$dv <- "Candidate Evaluation"

# plot predicted probabilities for m2f_res and m2g_res
ggplot(rbind(m2g_res,m2f_res), aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Independent Variable: Moral Foundation", x= "Change in Feeling Thermometer") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Feeling Thermometer Differentials") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for Party Identification")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=1:4, labels=c("Authority /\nRespect", "Ingroup / \nLoyalty"
                                        , "Fairness / \nReciprocity", "Harm / \nCare")) +
    facet_grid(dv~.)
ggsave(filename = "fig/m2f_vote.pdf")


### models predicting candidate evaluations based on mftXtraits

# model estimation
m2h_2008a <- zelig(eval_cand ~ harm_all*trait_moral + fair_all*trait_moral + ingr_all*trait_moral + auth_all*trait_moral + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2h_2012a <- zelig(eval_cand ~ harm_all*trait_moral + fair_all*trait_moral + ingr_all*trait_moral + auth_all*trait_moral + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

m2h_2008b <- zelig(eval_cand ~ harm_all*trait_lead + fair_all*trait_lead + ingr_all*trait_lead + auth_all*trait_lead + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2h_2012b <- zelig(eval_cand ~ harm_all*trait_lead + fair_all*trait_lead + ingr_all*trait_lead + auth_all*trait_lead + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

m2h_2008c <- zelig(eval_cand ~ harm_all*trait_care + fair_all*trait_care + ingr_all*trait_care + auth_all*trait_care + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2h_2012c <- zelig(eval_cand ~ harm_all*trait_care + fair_all*trait_care + ingr_all*trait_care + auth_all*trait_care + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

m2h_2008d <- zelig(eval_cand ~ harm_all*trait_know + fair_all*trait_know + ingr_all*trait_know + auth_all*trait_know + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2h_2012d <- zelig(eval_cand ~ harm_all*trait_know + fair_all*trait_know + ingr_all*trait_know + auth_all*trait_know + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

m2h_2008e <- zelig(eval_cand ~ harm_all*trait_int + fair_all*trait_int + ingr_all*trait_int + auth_all*trait_int + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2h_2012e <- zelig(eval_cand ~ harm_all*trait_int + fair_all*trait_int + ingr_all*trait_int + auth_all*trait_int + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

m2h_2008f <- zelig(eval_cand ~ harm_all*trait_honst + fair_all*trait_honst + ingr_all*trait_honst + auth_all*trait_honst + relig + educ + age + female + black, data=anes2008, model="ls",cite=F, robust=T)
m2h_2012f <- zelig(eval_cand ~ harm_all*trait_honst + fair_all*trait_honst + ingr_all*trait_honst + auth_all*trait_honst + relig + educ + age + female + black, data=anes2012, model="ls",cite=F, robust=T)

# generate table
stargazer(m2h_2008a,m2h_2012a,m2h_2008b,m2h_2012b,m2h_2008c,m2h_2012c,m2h_2008d,m2h_2012d,m2h_2008e,m2h_2012e,m2h_2008f,m2h_2012f
          , type="text", out="tab/m2h_vote.html"
          , title="Linear Model Predicting Feeling Thermometer Differential (Candidates)"
          , column.labels = rep(c("2008","2012"),6), model.numbers = TRUE
          , dep.var.labels="Feeling Thermometer Differential (preference for Democratic Candidate)"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , no.space=T
)


### models predicting vote choice based on mftXtraits

# model estimation
m2i_2008a <- zelig(vote_dem ~ harm_all*trait_moral + fair_all*trait_moral + ingr_all*trait_moral + auth_all*trait_moral + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2i_2012a <- zelig(vote_dem ~ harm_all*trait_moral + fair_all*trait_moral + ingr_all*trait_moral + auth_all*trait_moral + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

m2i_2008b <- zelig(vote_dem ~ harm_all*trait_lead + fair_all*trait_lead + ingr_all*trait_lead + auth_all*trait_lead + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2i_2012b <- zelig(vote_dem ~ harm_all*trait_lead + fair_all*trait_lead + ingr_all*trait_lead + auth_all*trait_lead + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

m2i_2008c <- zelig(vote_dem ~ harm_all*trait_care + fair_all*trait_care + ingr_all*trait_care + auth_all*trait_care + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2i_2012c <- zelig(vote_dem ~ harm_all*trait_care + fair_all*trait_care + ingr_all*trait_care + auth_all*trait_care + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

m2i_2008d <- zelig(vote_dem ~ harm_all*trait_know + fair_all*trait_know + ingr_all*trait_know + auth_all*trait_know + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2i_2012d <- zelig(vote_dem ~ harm_all*trait_know + fair_all*trait_know + ingr_all*trait_know + auth_all*trait_know + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

m2i_2008e <- zelig(vote_dem ~ harm_all*trait_int + fair_all*trait_int + ingr_all*trait_int + auth_all*trait_int + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2i_2012e <- zelig(vote_dem ~ harm_all*trait_int + fair_all*trait_int + ingr_all*trait_int + auth_all*trait_int + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

m2i_2008f <- zelig(vote_dem ~ harm_all*trait_honst + fair_all*trait_honst + ingr_all*trait_honst + auth_all*trait_honst + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2i_2012f <- zelig(vote_dem ~ harm_all*trait_honst + fair_all*trait_honst + ingr_all*trait_honst + auth_all*trait_honst + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

# generate table
stargazer(m2i_2008a,m2i_2012a,m2i_2008b,m2i_2012b,m2i_2008c,m2i_2012c,m2i_2008d,m2i_2012d,m2i_2008e,m2i_2012e,m2i_2008f,m2i_2012f
          , type="text", out="tab/m2i_vote.html"
          , title="Logit Model Predicting Vote Choice for Democratic Candidate"
          , column.labels = rep(c("2008","2012"),6), model.numbers = TRUE
          , dep.var.labels="Vote for Democratic Candidate"
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , no.space=T
)


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
  ggtitle("Change in Predicted Probabilities to Reference\nany Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for Both Remaining Variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=3:1, labels=c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions"))
ggsave(filename = "fig/m3_learn.pdf")


### models predicting the reference to specific moral foundations based on the interaction of ideology and political knowledge

# estimate models
m3b_2008_harm_polknow <- zelig(harm_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_harm_polknow <- zelig(harm_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_fair_polknow <- zelig(fair_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_fair_polknow <- zelig(fair_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_ingr_polknow <- zelig(ingr_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_ingr_polknow <- zelig(ingr_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_auth_polknow <- zelig(auth_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_auth_polknow <- zelig(auth_all ~ polknow_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m3b_2008_harm_polmedia <- zelig(harm_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_harm_polmedia <- zelig(harm_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_fair_polmedia <- zelig(fair_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_fair_polmedia <- zelig(fair_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_ingr_polmedia <- zelig(ingr_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_ingr_polmedia <- zelig(ingr_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_auth_polmedia <- zelig(auth_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_auth_polmedia <- zelig(auth_all ~ polmedia_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m3b_2008_harm_poldisc <- zelig(harm_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_harm_poldisc <- zelig(harm_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_fair_poldisc <- zelig(fair_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_fair_poldisc <- zelig(fair_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_ingr_poldisc <- zelig(ingr_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_ingr_poldisc <- zelig(ingr_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_auth_poldisc <- zelig(auth_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_auth_poldisc <- zelig(auth_all ~ poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

m3b_2008_harm_all <- zelig(harm_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_harm_all <- zelig(harm_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_fair_all <- zelig(fair_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_fair_all <- zelig(fair_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_ingr_all <- zelig(ingr_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_ingr_all <- zelig(ingr_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3b_2008_auth_all <- zelig(auth_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3b_2012_auth_all <- zelig(auth_all ~ polknow_c*ideol + polmedia_c*ideol + poldisc_c*ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

# generate table (including all 3 vars)
stargazer(m3b_2008_harm_all,m3b_2012_harm_all,m3b_2008_fair_all,m3b_2012_fair_all,m3b_2008_ingr_all,m3b_2012_ingr_all,m3b_2008_auth_all,m3b_2012_auth_all
          , type="text", out="tab/m3b_learn.tex"
          , title="Logit Models Predicting References to Specific Moral Foundations"
          , column.labels = rep(c("2008","2012"),4), model.numbers = TRUE
          , align=T, column.sep.width="-15pt", digits=3, digits.extra=1, font.size="tiny"
          , label="tab:m3b_learn", no.space=T, table.placement="ht"
          )

# plot the results (UPDATE CODE IN OTHER PLOTS ACCORDINGLY?)
m3b_res <- data.frame()
mlist <- list(m3b_2008_harm_polknow,m3b_2012_harm_polknow,m3b_2008_fair_polknow,m3b_2012_fair_polknow,m3b_2008_ingr_polknow,m3b_2012_ingr_polknow,m3b_2008_auth_polknow,m3b_2012_auth_polknow
            , m3b_2008_harm_polmedia,m3b_2012_harm_polmedia,m3b_2008_fair_polmedia,m3b_2012_fair_polmedia,m3b_2008_ingr_polmedia,m3b_2012_ingr_polmedia,m3b_2008_auth_polmedia,m3b_2012_auth_polmedia
            , m3b_2008_harm_poldisc,m3b_2012_harm_poldisc,m3b_2008_fair_poldisc,m3b_2012_fair_poldisc,m3b_2008_ingr_poldisc,m3b_2012_ingr_poldisc,m3b_2008_auth_poldisc,m3b_2012_auth_poldisc)
for(i in 1:length(mlist)){
    x <- setx(mlist[[i]]
            , polknow_c  = rep(rev(range(mlist[[i]]$data[,"polknow_c"])), 2)
            , polmedia_c = rep(rev(range(mlist[[i]]$data[,"polmedia_c"])), 2)
            , poldisc_c  = rep(rev(range(mlist[[i]]$data[,"poldisc_c"])), 2)
            , ideol = rep(c("Liberal","Conservative"), each = 2))
    sim <- sim(mlist[[i]], x = x)
    sim <- (sim$qi$ev[,1] - sim$qi$ev[,3]) - (sim$qi$ev[,2] - sim$qi$ev[,4])
    new <- data.frame(mean = mean(sim), cilo = quantile(sim, 0.025)
                    , cihi = quantile(sim, 0.975), dv = all.vars(mlist[[i]]$formula)[1]
                     , iv = all.vars(mlist[[i]]$formula)[2]
                              , year = mlist[[i]]$data[,"year"][1], cond = "No")
    m3b_res <- rbind(m3b_res, new)
}
mlist <- list(m3b_2008_harm_all,m3b_2012_harm_all,m3b_2008_fair_all,m3b_2012_fair_all,m3b_2008_ingr_all,m3b_2012_ingr_all,m3b_2008_auth_all,m3b_2012_auth_all)
for(i in 1:length(mlist)){
    x <- setx(mlist[[i]]
            , polknow_c  = c(rep(rev(range(mlist[[i]]$data[,"polknow_c"])), 2), rep(mean(mlist[[i]]$data[,"polknow_c"]), 8))
            , polmedia_c = c(rep(mean(mlist[[i]]$data[,"polmedia_c"]), 4), rep(rev(range(mlist[[i]]$data[,"polmedia_c"])), 2), rep(mean(mlist[[i]]$data[,"polmedia_c"]), 4))
            , poldisc_c  = c(rep(mean(mlist[[i]]$data[,"poldisc_c"]), 8), rep(rev(range(mlist[[i]]$data[,"poldisc_c"])), 2))
            , ideol = rep(c("Liberal","Liberal","Conservative","Conservative"), 3))
    sim <- sim(mlist[[i]], x = x)
    sim <- cbind((sim$qi$ev[,1] - sim$qi$ev[,3]) - (sim$qi$ev[,2] - sim$qi$ev[,4])
               , (sim$qi$ev[,5] - sim$qi$ev[,7]) - (sim$qi$ev[,6] - sim$qi$ev[,8])
               , (sim$qi$ev[,9] - sim$qi$ev[,11]) - (sim$qi$ev[,10] - sim$qi$ev[,12]))
    new <- data.frame(mean = apply(sim,2,mean), cilo = apply(sim, 2, quantile, 0.025)
                    , cihi = apply(sim, 2, quantile, 0.975), dv = all.vars(mlist[[i]]$formula)[1]
                     , iv = c("polknow_c", "polmedia_c", "poldisc_c")
                    , year = mlist[[i]]$data[,"year"][1], cond = "Yes")
    m3b_res <- rbind(m3b_res, new)
}
m3b_res$var <- - as.numeric(m3b_res$iv) + 4
m3b_res$year <- as.character(m3b_res$year)
levels(m3b_res$dv) <- c("Harm / Care", "Fairness / Reciprocity", "Ingroup / Loyalty", "Authority / Respect")
ggplot(m3b_res, aes(x = mean, y = var-.1+.3*(year=="2008")-.1*(cond=="Yes"), shape=year, color = year, lty=cond)) +
  geom_point(size=4) + geom_errorbarh(aes(xmax=cihi,xmin=cilo),height=.1) + 
  labs(y = "Moderating Variable", x= "Change in Effect of Ideology (Liberal - Conservative)") + geom_vline(xintercept=0) + 
  theme_bw() + scale_color_manual(values=c("royalblue", "firebrick")) +
  ggtitle("Change in Effect of Ideology on the\nProbability to Reference each Moral Foundation") +
  guides(color=guide_legend(title="Survey Year"), shape=guide_legend(title="Survey Year"), lty=guide_legend(title="Control for Both Remaining Variables")) +
  theme(legend.position="bottom", legend.box="horizontal") + 
  scale_y_continuous(breaks=3:1, labels=c("Political\nKnowledge","Political Media\nExposure","Political\nDiscussions")) + facet_wrap(~dv)
ggsave(filename = "fig/m3b_learn.pdf")


### look at relationship between moral foundations

## correlation matrix
stargazer(cor(anes2008[,c("harm_all","fair_all","ingr_all","auth_all","puri_all")]
            , use="pairwise.complete.obs")
        , type = "text", out = "tab/cor2008.tex", label = "tab:cor2008")
stargazer(cor(anes2012[,c("harm_all","fair_all","ingr_all","auth_all","puri_all")]
            , use="pairwise.complete.obs")
        , type = "text", out = "tab/cor2012.tex", label = "tab:cor2012")

## factor analyses of MFT dimensions
fact2008 <- factanal(na.omit(anes2008[,c("harm_all","fair_all","ingr_all","auth_all","puri_all")])
                    , 2, rotation="varimax")
print(fact2008, digits=2, cutoff=.2, sort=TRUE)

fact2012 <- factanal(na.omit(anes2012[,c("harm_all","fair_all","ingr_all","auth_all","puri_all")])
                    , 2, rotation="varimax")
print(fact2012, digits=2, cutoff=.2, sort=TRUE)





