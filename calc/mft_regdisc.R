###########################################################################################
## Project:  Moral foundations of Political Reasoning
## File:     mft_regdisc.R
## Overview: supplementary regression discontinuity analyses
## Author:   Patrick Kraft
###########################################################################################


rm(list=ls())
setwd("/data/Uni/projects/2014/mft/calc")

# load packages
pkg <- c("reshape2","ggplot2","stargazer","xtable","rdd","rdrobust")
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


### regression discontinuity design investigating the effect of previous voting

# plot discontinuity for aggregate data
anes2008regdi <- aggregate(anes2008$mft_all,by=list(anes2008$regdi_year),FUN=mean,na.rm=T)
anes2008regdi$cond <- anes2008regdi$Group.1 >= 18
rd1a <- ggplot(anes2008regdi, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2008 Survey)") +
  geom_smooth() + theme_bw()
rd1b <- ggplot(anes2008regdi, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Linear Fit (2008 Survey)") +
  theme_bw() + geom_smooth(method=lm)
rd1c <- ggplot(anes2008regdi, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Quadratic Fit (2008 Survey)") +
  geom_smooth(method=lm, formula = y~poly(x,2)) + theme_bw()

anes2012regdi <- aggregate(anes2012$mft_all,by=list(anes2012$regdi_year),FUN=mean,na.rm=T)
anes2012regdi$cond <- anes2012regdi$Group.1 >= 18
rd1d <- ggplot(anes2012regdi, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2012 Survey)") +
  geom_smooth() + theme_bw()
rd1e <- ggplot(anes2012regdi, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Linear Fit (2012 Survey)") +
  theme_bw() + geom_smooth(method=lm)
rd1f <- ggplot(anes2012regdi, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Referencing Moral Foundations") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Quadratic Fit (2012 Survey)") +
  theme_bw() + geom_smooth(method=lm, formula = y~poly(x,2))
pdf("fig/rd1_overview.pdf")
multiplot(rd1a, rd1b, rd1d, rd1e, cols=2)
dev.off()
# question: calculate simple mean differences for regression discontinuity?
# I should write a function for that

# sharp discontinuity (ITT effect of prior eligibility)
m4y_2008 <- RDestimate(mft_all ~ regdi_year, data = anes2008, cutpoint = 17.5)
summary(m4y_2008)
plot(m4y_2008)
m4y_2012 <- RDestimate(mft_all ~ regdi_year, data = anes2012, cutpoint = 17.5)
summary(m4y_2012)
plot(m4y_2012)
m4m_2008a <- RDestimate(mft_all ~ regdi_month, data = anes2008, cutpoint = 0.5)
summary(m4m_2008a)
plot(m4m_2008a)
m4m_2008b <- RDestimate(mft_all ~ regdi_month, data = anes2008, cutpoint = 0.5, bw=20)
summary(m4m_2008b)
plot(m4m_2008b)
rd_tab <- function(model,title,file,lab){
  tab <- cbind(model$bw,model$obs,model$est,model$se,model$p,m4y_2008$ci)
  colnames(tab) <- c("Bandwidth","Obs.","Est.","SE","Pr(>|z|)","CI (low)","CI (high)")
  print(xtable(tab, align="lrrrrrrr",digits=c(0,4,0,4,4,4,4,4), caption = title, label = lab),file=file)
}
rd_tab(m4y_2008, title="Regression Discontinuity Estimates Based on Age (2008)"
       , file="tab/rd2008y.tex", lab="tab:rd2008y")
rd_tab(m4y_2012, title="Regression Discontinuity Estimates Based on Age (2012)"
       , file="tab/rd2012y.tex", lab="tab:rd2012y")
rd_tab(m4m_2008a, title="Regression Discontinuity Estimates Based on Month of Birth (2008)"
       , file="tab/rd2008m1.tex", lab="tab:rd2008m1")
rd_tab(m4m_2008b, title="Regression Discontinuity Estimates Based on Month of Birth (2008)"
       , file="tab/rd2008m2.tex", lab="tab:rd2008m2")

# density of rating variable
rd2a <- ggplot(anes2008, aes(x=regdi_year)) + geom_density() +
  scale_y_continuous(name="Density") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  ggtitle("2008 Survey, Discontinuity Based on Age") +
  geom_vline(aes(xintercept=17.5)) + theme_bw()
rd2b <- ggplot(anes2012, aes(x=regdi_year)) + geom_density() +
  scale_y_continuous(name="Density") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  ggtitle("2012 Survey, Discontinuity Based on Age") +
  geom_vline(aes(xintercept=17.5)) + theme_bw()
rd2c <- ggplot(anes2008, aes(x=regdi_month)) + geom_density() +
  scale_y_continuous(name="Density") +
  scale_x_continuous(name="Number of Months Respondent has been\n18 Years Old Before the Previous Election") +
  ggtitle("2008 Survey, Discontinuity Based Month of Birth") +
  geom_vline(aes(xintercept=-0.5)) + theme_bw()
pdf("fig/rd2_density.pdf")
multiplot(rd2a, rd2b, rd2c, cols=1)
dev.off()

# placebo test
m4y_2008 <- RDestimate(mft_all ~ regdi_year, data = anes2008, cutpoint = 21.5)
m4y_2012 <- RDestimate(mft_all ~ regdi_year, data = anes2012, cutpoint = 21.5)
m4m_2008 <- RDestimate(mft_all ~ regdi_month, data = anes2008, cutpoint = 47.5, bw=20)
rd_tab(m4y_2008, title="Regression Discontinuity Estimates Based on Age (2008) - Placebo Test using Different Cutoff (4 years later)"
       , file="tab/rd2008y_plac.tex", lab="tab:rd2008y_plac")
rd_tab(m4y_2012, title="Regression Discontinuity Estimates Based on Age (2012) - Placebo Test using Different Cutoff (4 years later)"
       , file="tab/rd2012y_plac.tex", lab="tab:rd2012y_plac")
rd_tab(m4m_2008, title="Regression Discontinuity Estimates Based on Month of Birth (2008) - Placebo Test using Different Cutoff (48 months later)"
       , file="tab/rd2008m_plac.tex", lab="tab:rd2008m_plac")

# non-outcome
m4y_2008 <- RDestimate(issue_gay ~ regdi_year, data = anes2008, cutpoint = 17.5)
m4y_2012 <- RDestimate(issue_gay ~ regdi_year, data = anes2012, cutpoint = 17.5)
m4m_2008 <- RDestimate(issue_gay ~ regdi_month, data = anes2008, cutpoint = -0.5, bw=20)
rd_tab(m4y_2008, title="Regression Discontinuity Estimates Based on Age (2008) - Validity Test using Non-Outcome"
       , file="tab/rd2008y_non.tex", lab="tab:rd2008y_non")
rd_tab(m4y_2012, title="Regression Discontinuity Estimates Based on Age (2012) - Validity Test using Non-Outcome"
       , file="tab/rd2012y_non.tex", lab="tab:rd2012y_non")
rd_tab(m4m_2008, title="Regression Discontinuity Estimates Based on Month of Birth (2008) - Validity Test using Non-Outcome"
       , file="tab/rd2008m_non.tex", lab="tab:rd2008m_non")


### optional: fuzzy discontinuity  (ATT effect of prior vote)

m4m_2008p <- rdrobust(anes2008$mft_all, anes2008$regdi_month, c=-0.5, p=2, q=3)
summary(m4m_2008p)
rdplot(anes2008$mft_all, anes2008$regdi_month, c=-0.5,p=2)

m4m_2008p <- rdrobust(anes2008$mft_all, anes2008$regdi_month, fuzzy=anes2008$pastvote, c=-0.5, p=2, q=3)
summary(m4m_2008p)
rdplot(anes2008$mft_all, anes2008$regdi_month, c=-0.5, p=2)


### regression discontinuity design investigating the effect of previous voting
### Version 2: percentage of total 'moral' words in each answer

# plot discontinuity for aggregate data
anes2008regdi2 <- aggregate(anes2008$num_prop,by=list(anes2008$regdi_year),FUN=mean,na.rm=T)
anes2008regdi2$cond <- anes2008regdi2$Group.1 >= 18
rd1a <- ggplot(anes2008regdi2, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2008 Survey)") +
  geom_smooth() + theme_bw()
rd1b <- ggplot(anes2008regdi2, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Linear Fit (2008 Survey)") +
  theme_bw() + geom_smooth(method=lm)
rd1c <- ggplot(anes2008regdi2, aes(x=Group.1, y=x, color=cond, shape=cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Quadratic Fit (2008 Survey)") +
  geom_smooth(method=lm, formula = y~poly(x,2)) + theme_bw()

anes2012regdi2 <- aggregate(anes2012$num_prop,by=list(anes2012$regdi_year),FUN=mean,na.rm=T)
anes2012regdi2$cond <- anes2012regdi2$Group.1 >= 18
rd1d <- ggplot(anes2012regdi2, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2012 Survey)") +
  geom_smooth() + theme_bw()
rd1e <- ggplot(anes2012regdi2, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Linear Fit (2012 Survey)") +
  theme_bw() + geom_smooth(method=lm)
rd1f <- ggplot(anes2012regdi2, aes(x=Group.1, y=x, color=cond, shape = cond)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Quadratic Fit (2012 Survey)") +
  theme_bw() + geom_smooth(method=lm, formula = y~poly(x,2))
pdf("fig/Xrd1_overview.pdf")
multiplot(rd1a, rd1b, rd1d, rd1e, cols=2)
dev.off()
# question: calculate simple mean differences for regression discontinuity?
# I should write a function for that

# sharp discontinuity (ITT effect of prior eligibility)
m4y_2008X <- RDestimate(num_prop ~ regdi_year, data = anes2008, cutpoint = 17.5, bw=2.5)
summary(m4y_2008X)
plot(m4y_2008X)
m4y_2012X <- RDestimate(num_prop ~ regdi_year, data = anes2012, cutpoint = 17.5, bw=2.5)
summary(m4y_2012X)
plot(m4y_2012X)
m4m_2008aX <- RDestimate(num_prop ~ regdi_month, data = anes2008, cutpoint = 0.5, bw=5)
summary(m4m_2008aX)
plot(m4m_2008aX)
m4m_2008bX <- RDestimate(num_prop ~ regdi_month, data = anes2008, cutpoint = 0.5, bw=20)
summary(m4m_2008bX)
plot(m4m_2008bX)
rd_tab(m4y_2008X, title="Regression Discontinuity Estimates Based on Age (2008) - Proportion of Moral Words"
       , file="tab/Xrd2008y.tex", lab="tab:Xrd2008y")
rd_tab(m4y_2012X, title="Regression Discontinuity Estimates Based on Age (2012) - Proportion of Moral Words"
       , file="tab/Xrd2012y.tex", lab="tab:Xrd2012y")
rd_tab(m4m_2008aX, title="Regression Discontinuity Estimates Based on Month of Birth (2008) - Proportion of Moral Words"
       , file="tab/Xrd2008m1.tex", lab="tab:Xrd2008m1")
rd_tab(m4m_2008bX, title="Regression Discontinuity Estimates Based on Month of Birth (2008) - Proportion of Moral Words"
       , file="tab/Xrd2008m2.tex", lab="tab:Xrd2008m2")

# placebo test
m4y_2008X <- RDestimate(num_prop ~ regdi_year, data = anes2008, cutpoint = 21.5, bw=2.5)
m4y_2012X <- RDestimate(num_prop ~ regdi_year, data = anes2012, cutpoint = 21.5, bw=2.5)
m4m_2008X <- RDestimate(num_prop ~ regdi_month, data = anes2008, cutpoint = 47.5, bw=20)
rd_tab(m4y_2008X, title="Regression Discontinuity Estimates Based on Age (2008) - Placebo Test using Different Cutoff (4 years later) - Proportion of Moral Words"
       , file="tab/Xrd2008y_plac.tex", lab="tab:Xrd2008y_plac")
rd_tab(m4y_2012X, title="Regression Discontinuity Estimates Based on Age (2012) - Placebo Test using Different Cutoff (4 years later) - Proportion of Moral Words"
       , file="tab/Xrd2012y_plac.tex", lab="tab:Xrd2012y_plac")
rd_tab(m4m_2008X, title="Regression Discontinuity Estimates Based on Month of Birth (2008) - Placebo Test using Different Cutoff (48 months later) - Proportion of Moral Words"
       , file="tab/Xrd2008m_plac.tex", lab="tab:Xrd2008m_plac")

### optional: fuzzy discontinuity  (ATT effect of prior vote)

m4m_2008pX <- rdrobust(anes2008$num_prop, anes2008$regdi_month, c=-0.5, p=2, q=3)
summary(m4m_2008pX)
rdplot(anes2008$num_prop, anes2008$regdi_month, c=-0.5,p=2)

m4m_2008pX <- rdrobust(anes2008$num_prop, anes2008$regdi_month, fuzzy=anes2008$pastvote, c=-0.5, p=2, q=3)
summary(m4m_2008pX)
rdplot(anes2008$num_prop, anes2008$regdi_month, c=-0.5, p=2)



### regression discontinuity design investigating the effect of previous voting
### Version 3: percentage of total 'moral' words in each answer, plot without aggregating


rd1a <- ggplot(anes2008, aes(x=regdi_year, y=num_prop, color=regdi_year>=18, shape=regdi_year>=18)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2008 Survey)") +
      geom_smooth(method=lm) + theme_bw()
rd2a <- ggplot(anes2012, aes(x=regdi_year, y=num_prop, color=regdi_year>=18, shape=regdi_year>=18)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2012 Survey)") +
      geom_smooth(method=lm) + theme_bw()
# Note that there is a bug in the percentage of total words since there are values >1
# there is either a problem in the total word count or the nmoral word count...


### regression discontinuity design investigating the effect of previous voting
### Version 4: combine 2008 and 2012

rd3a <- ggplot(merge(anes2008,anes2012, all =T), aes(x=regdi_year, y=num_prop, color=regdi_year>=18, shape=regdi_year>=18)) + geom_point() +
  scale_color_manual(values=c("royalblue", "firebrick"),guide=FALSE) +
  scale_shape(guide=FALSE) + scale_y_continuous(name="% Moral Words") +
  scale_x_continuous(name="Age at the Time of the Previous Election") +
  geom_vline(aes(xintercept=17.5),color="grey") + ggtitle("Loess Fit (2008 Survey)") +
      geom_smooth(method=lm) + theme_bw()


test <- RDestimate(num_prop ~ regdi_year, data = merge(anes2008,anes2012, all =T), cutpoint = 17.5, bw=2.5)
summary(test)


#####################################################
### regdisc using new similarity measure


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




