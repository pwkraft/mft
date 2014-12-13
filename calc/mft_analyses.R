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
pkg <- c("reshape2","ggplot2","stargazer","xtable","rdd","rdrobust")
inst <- pkg %in% installed.packages()  
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
lapply(pkg,function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
rm(list=ls())

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

tab_mis <- rbind(c(table(anes2008$spanish),table(anes2008$spanish)[2]*100/sum(table(anes2008$spanish)))
                 , c(table(anes2012$spanish),table(anes2012$spanish)[2]*100/sum(table(anes2012$spanish)))
                 , c(table(anes2008$num_total==0),table(anes2008$num_total==0)[2]*100/sum(table(anes2008$num_total==0)))
                 , c(table(anes2012$num_total==0),table(anes2012$num_total==0)[2]*100/sum(table(anes2012$num_total==0)))
                 , c(table(anes2008$num_ca==0),table(anes2008$num_ca==0)[2]*100/sum(table(anes2008$num_ca==0)))
                 , c(table(anes2012$num_ca==0),table(anes2012$num_ca==0)[2]*100/sum(table(anes2012$num_ca==0)))
                 , c(table(anes2008$num_pa==0),table(anes2008$num_pa==0)[2]*100/sum(table(anes2008$num_pa==0)))
                 , c(table(anes2012$num_pa==0),table(anes2012$num_pa==0)[2]*100/sum(table(anes2012$num_pa==0))))
colnames(tab_mis) <- c("No","Yes","Percent (Yes)")
rownames(tab_mis) <- c("Spanish Interview (2008)", "(2012)", "No Responses (Overall, 2008)", "No Responses (Overall, 2012)"
                       , "No Responses (Candidate Evaluations, 2008)", "No Responses (Candidate Evaluations, 2012)"
                       , "No Responses (Party Evaluations, 2008)", "No Responses (Party Evaluations, 2012)")
print(xtable(tab_mis, align="lccc",digits=c(0,0,0,2)),file="tab/a1_tab_mis.tex")


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
### control for log(num_total)!!!

### models predicting references to moral foundations (without interactions)

m1_2008_harm <- zelig(harm_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2008_fair <- zelig(fair_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2008_ingr <- zelig(ingr_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2008_auth <- zelig(auth_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m1_2012_harm <- zelig(harm_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1_2012_fair <- zelig(fair_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1_2012_ingr <- zelig(ingr_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m1_2012_auth <- zelig(auth_all ~ ideol + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
stargazer(m1_2008_harm,m1_2008_fair,m1_2008_ingr,m1_2008_auth,m1_2012_harm,m1_2012_fair,m1_2012_ingr,m1_2012_auth
          , type="text"#, out="m1_all.tex"
          #, title="Logit Models Predicting overall References to Moral Foundations"
          #, covariate.labels=c("Conservative","Moderate","Political Interest"
          #                     ,"Conservative X Political Interest","Moderate X Political Interest"
          #                     ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          #, order=c(1:3,9,10,4:8), dep.var.labels="Any Moral Foundation"
          #, align=T, column.sep.width="1pt", digits=3, digits.extra=1, font.size="scriptsize"
          #, label="tab:m1_all", no.space=T#, table.placement="ht"
)

### models predicting vote choice based on moral considerations

m2_2008_vote1 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2_2008_vote2 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2008, model="logit",cite=F)
m2_2012_vote1 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)
m2_2012_vote2 <- zelig(vote_dem ~ harm_all + fair_all + ingr_all + auth_all + pid_dem + pid_rep + relig + educ + age + female + black, data=anes2012, model="logit",cite=F)

stargazer(m2_2008_vote1,m2_2008_vote2,m2_2012_vote1,m2_2012_vote2
          , type="text"#, out="m1_all.tex"
          #, title="Logit Models Predicting overall References to Moral Foundations"
          #, covariate.labels=c("Conservative","Moderate","Political Interest"
          #                     ,"Conservative X Political Interest","Moderate X Political Interest"
          #                     ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          #, order=c(1:3,9,10,4:8), dep.var.labels="Any Moral Foundation"
          #, align=T, column.sep.width="1pt", digits=3, digits.extra=1, font.size="scriptsize"
          #, label="tab:m1_all", no.space=T#, table.placement="ht"
)

### models including interactions with political knowledge, media exposure, political discussions
# for now, I will only concentrate on mft_all here!

m3_2008_polknow <- zelig(mft_all ~ polknow + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2008_polmedia <- zelig(mft_all ~ polmedia + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2008_poldisc <- zelig(mft_all ~ poldisc + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2008_all <- zelig(mft_all ~ polknow + polmedia + poldisc + relig + educ + age + female + black + num_total, data=anes2008, model="logit",cite=F)
m3_2012_polknow <- zelig(mft_all ~ polknow + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3_2012_polmedia <- zelig(mft_all ~ polmedia + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3_2012_poldisc <- zelig(mft_all ~ poldisc + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)
m3_2012_all <- zelig(mft_all ~ polknow + polmedia + poldisc + relig + educ + age + female + black + num_total, data=anes2012, model="logit",cite=F)

stargazer(m3_2008_polknow, m3_2008_polmedia, m3_2008_poldisc, m3_2008_all, m3_2012_polknow, m3_2012_polmedia, m3_2012_poldisc, m3_2012_all
          , type="text"#, out="m1_all.tex"
          #, title="Logit Models Predicting overall References to Moral Foundations"
          #, covariate.labels=c("Conservative","Moderate","Political Interest"
          #                     ,"Conservative X Political Interest","Moderate X Political Interest"
          #                     ,"Church Attendance","Education (College Degree)","Age","Sex (Female)","Race (African American)")
          #, order=c(1:3,9,10,4:8), dep.var.labels="Any Moral Foundation"
          #, align=T, column.sep.width="1pt", digits=3, digits.extra=1, font.size="scriptsize"
          #, label="tab:m1_all", no.space=T#, table.placement="ht"
)

### regression discontinuity design investigating the effect of previous voting

# plot discontinuity for aggregate data

anes2008regdi <- aggregate(anes2008$mft_all,by=list(anes2008$regdi_year),FUN=mean,na.rm=T)
anes2008regdi$cond <- anes2008regdi$Group.1 >= 18
ggplot(anes2008regdi, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick")) +
  theme_bw() + geom_smooth(method=lm)
ggplot(anes2008regdi, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick")) +
  geom_smooth() + theme_bw()
ggplot(anes2008regdi, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick")) +
  geom_smooth(method=lm, formula = y~poly(x,2)) + theme_bw()



anes2012regdi <- aggregate(anes2012$mft_all,by=list(anes2012$regdi_year),FUN=mean,na.rm=T)
anes2012regdi$cond <- anes2012regdi$Group.1 >= 18
ggplot(anes2012regdi, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick")) +
  geom_smooth() + theme_bw()
ggplot(anes2012regdi, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick")) +
  theme_bw() + geom_smooth(method=lm)
ggplot(anes2012regdi, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick")) +
  theme_bw() + geom_smooth(method=lm, formula = y~poly(x,2))



### sharp discontinuity (ITT effect of prior eligibility)

m4y_2008 <- RDestimate(mft_all ~ regdi_year, data = anes2008, cutpoint = 17.5)
summary(m4y_2008)
plot(m4y_2008)

m4y_2012 <- RDestimate(mft_all ~ regdi_year, data = anes2012, cutpoint = 17.5)
summary(m4y_2012)
plot(m4y_2012)

m4m_2008 <- RDestimate(mft_all ~ regdi_month, data = anes2008, cutpoint = -0.5, bw=20)
summary(m4m_2008)
plot(m4m_2008)


### density of rating variable

ggplot(anes2008, aes(x=regdi_year)) + geom_density() +
  geom_vline(aes(xintercept=17.5)) + theme_bw()
ggplot(anes2008, aes(x=regdi_month)) + geom_density() +
  geom_vline(aes(xintercept=-0.5)) + theme_bw()
ggplot(anes2012, aes(x=regdi_year)) + geom_density() +
  geom_vline(aes(xintercept=17.5)) + theme_bw()


### placebo test

m4y_2008 <- RDestimate(mft_all ~ regdi_year, data = anes2008, cutpoint = 21.5)
summary(m4y_2008)

m4y_2012 <- RDestimate(mft_all ~ regdi_year, data = anes2012, cutpoint = 21.5)
summary(m4y_2012)

m4m_2008 <- RDestimate(mft_all ~ regdi_month, data = anes2008, cutpoint = 47.5, bw=20)
summary(m4m_2008)


### non-outcome

m4y_2008 <- RDestimate(issue_gay ~ regdi_year, data = anes2008, cutpoint = 17.5)
summary(m4y_2008)
plot(m4y_2008)

m4y_2012 <- RDestimate(issue_gay ~ regdi_year, data = anes2012, cutpoint = 17.5)
summary(m4y_2012)
plot(m4y_2012)

m4m_2008 <- RDestimate(issue_gay ~ regdi_month, data = anes2008, cutpoint = -0.5, bw=20)
summary(m4m_2008)
plot(m4m_2008)


## optional: fuzzy discontinuity  (ATT effect of prior vote)

m4m_2008p <- rdrobust(anes2008$mft_all, anes2008$regdi_month, c=-0.5, p=2, q=3)
summary(m4m_2008p)
rdplot(anes2008$mft_all, anes2008$regdi_month, c=-0.5,p=2)

m4m_2008p <- rdrobust(anes2008$mft_all, anes2008$regdi_month, fuzzy=anes2008$pastvote, c=-0.5, p=2, q=3)
summary(m4m_2008p)
rdplot(anes2008$mft_all, anes2008$regdi_month, c=-0.5, p=4)
